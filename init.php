<?php
class Elfeed_Sync extends Plugin {
	private $host;

	function about() {
		return array(null, "Elfeed Sync", "Sync API for elfeed", true);
	}

	function api_version() {
		return 2;
	}

	function init($host) {
		$this->host = $host;

		$this->host->add_api_method("setFeedsTree", $this);
		$this->host->add_api_method("syncElfeed", $this);
	}

	function createCategoriesTree($tree, $feed_categories = null, $current_cat = null) {
		if ($feed_categories === null) {
			$feed_categories = ORM::for_table('ttrss_feed_categories')
				->select_many('id', 'title', 'order_id', 'parent_cat')
				->where('owner_uid', $_SESSION['uid'])
				->order_by_asc('order_id')
				->order_by_asc('title')
				->find_many();
		}
		$categories = array();
		$feeds = array();
		foreach ($tree as $datum) {
			if (array_key_exists("group", $datum)) {
				$existing_cat = null;
				$title = $datum["group"]["name"];
				foreach ($feed_categories as $cat) {
					if ($cat->title === $title  && $cat->parent_cat === $current_cat) {
						$existing_cat = $cat->id;
						break;
					}
				}
				if (!$existing_cat) {
					$existing_cat = rand(-10000000, -1);
				}
				array_push($categories, array("id" => $existing_cat, "title" => $title, "parent_id" => $current_cat));
				list($child_categories, $child_feeds) = $this->createCategoriesTree(
					$datum["group"]["children"],
					$feed_categories,
					$existing_cat
				);
				$categories = array_merge($categories, $child_categories);
				$feeds = array_merge($feeds, $child_feeds);
			} else if (array_key_exists("feed", $datum)) {
				$url = $datum["feed"]["url"];
				$title = $datum["feed"]["title"];
				$tags = $datum["feed"]["tags"];
				array_push($feeds, array("url" => $url, "title" => $title, "tags" => $tags, "cat" => $current_cat));
			}
		}
		return array($categories, $feeds);
	}

	function mergeCategoriesTree($categories, $feeds) {
		$enable_cats = get_pref(Prefs::ENABLE_FEED_CATS);
		$cat_ids = array();
		if ($enable_cats) {
			foreach($categories as $cat_data) {
				if ($cat_data["id"] >= 0) {
					$cat_ids[$cat_data["id"]] = $cat_data["id"];
				} else {
					$cat = ORM::for_table('ttrss_feed_categories')->create();
					$parent_cat = $cat_ids[$cat_data["parent_id"]] ?? $cat_data["parent_id"] ?? null;
					$cat->set([
						'owner_uid' => $_SESSION['uid'],
						'parent_cat' => $parent_cat,
						'order_id' => 0,
						'title' => $cat_data['title']
					]);
					$cat->save();
					$cat_ids[$cat_data["id"]] = $cat->id;
				}
			}
		}
		$results = array();
		foreach($feeds as $feed_datum) {
			$cat_id = $cat_ids[$feed_datum["cat"]];
			$feed = ORM::for_table('ttrss_feeds')
				  ->where('feed_url', $feed_datum["url"])
				  ->where('owner_uid', $_SESSION['uid'])
				  ->find_one();
			if (!$feed) {
				$rc = Feeds::_subscribe($feed_datum["url"], $cat_id);
				if ($rc["code"] === 0 || $rc["code"] === 1) {
			        $feed = ORM::for_table('ttrss_feeds')
			        	  ->where('id', $rc['feed_id'])
			        	  ->where('owner_uid', $_SESSION['uid'])
			        	  ->find_one();
				}
			}
			if ($feed) {
				if (array_key_exists('title', $feed_datum)) {
					$feed->set([
						'title' => $feed_datum['title'],
					]);
				}
				$feed->set([
					'cat_id' => $cat_id
				]);
				$feed->save();
				$results[$feed_datum["url"]] = $feed->id;
			} else {
				$results[$feed_datum["url"]] = null;
			}
		}
		return $results;
	}

	function setFeedsTree() {
		$tree = $_REQUEST["tree"];

		list($categories, $feeds) = $this->createCategoriesTree($tree);
		$subscribed = $this->mergeCategoriesTree($categories, $feeds);

		return array(API::STATUS_OK, array('subscribed' => $subscribed));
	}

	function updateEntries($data) {
		$entries_query = ORM::for_table('ttrss_entries')
				 ->table_alias('e')
				 ->select_many('e.id', 'e.link', 'e.title', 'e.updated',
							   'ue.marked', 'ue.unread', 'ue.feed_id')
				 ->join('ttrss_user_entries', ['ue.ref_id', '=', 'e.id'], 'ue')
				 ->join('ttrss_feeds', ['f.id', '=', 'ue.feed_id'], 'f')
				 ->where('ue.owner_uid', $_SESSION['uid']);
		if (!is_null($data['look_back'])) {
			$entries_query = $entries_query->where_gte('updated', date('Y-m-d H:i:s', time() - $data['look_back']));
		}
		$entries = $entries_query->find_array();

		$feeds = ORM::for_table('ttrss_feeds')
			   ->select_many('id', 'feed_url')
			   ->where('owner_uid', $_SESSION['uid'])
			   ->find_array();

		$feed_by_link = array();
		foreach ($feeds as $feed) {
			$feed_by_link[$feed['feed_url']] = $feed['id'];
		}
		$bad_feed_ids = array();
		foreach($data['bad_feeds'] as $bad_feed_link) {
			$feed_id = $feed_by_link[$bad_feed_link];
			if ($feed_id) {
				$bad_feed_ids[$feed_id] = true;
			}
		}

		$entries_by_link = array();
		$entries_by_title_date = array();
		foreach ($entries as $entry) {
			if ($bad_feed_ids[$entry['feed_id']]) {
				$title_date = $entry['title'] . '---' . $entry['updated'];
				$entries_by_title_date[$title_date] = $entry;
			} else {
				$entries_by_link[$entry['link']] = $entry;
			}
		}

		$missing_feeds = array();
		$missing_entries = array();
		$toggle_unread = array();
		$toggle_marked = array();
		foreach($data['changed'] as $changed_entry) {
			$feed_id = $feed_by_link[$changed_entry['feed_url']];
			if (!$feed_id) {
				$missing_feeds[$changed_entry['feed_url']] = true;
				continue;
			}
			$bad_feed = $bad_feed_ids[$feed_id];
			$entry = null;
			if ($bad_feed) {
				$title_date = $changed_entry['title'] . '---' . $changed_entry['date'];
				$entry = $entries_by_title_date[$title_date];
			} else {
				$entry = $entries_by_link[$changed_entry['url']];
			}
			if (!$entry) {
				array_push($missing_entries, $changed_entry);
				continue;
			}

			if (!is_array($changed_entry['tags'])) {
				continue;
			}
			$unread = in_array($data['unread_tag'], $changed_entry['tags']);
			$marked = in_array($data['starred_tag'], $changed_entry['tags']);
			if ($entry['unread'] != $unread) {
				array_push($toggle_unread, $entry['id']);
			}
			if ($entry['marked'] != $marked) {
				array_push($toggle_marked, $entry['id']);
			}
		}

		ORM::get_db()->beginTransaction();
		if (count($toggle_unread) > 0) {
			ORM::raw_execute('UPDATE ttrss_user_entries t SET unread = not t.unread WHERE ref_id IN ('.implode(',', $toggle_unread).')');
		}
		if (count($toggle_marked) > 0) {
			ORM::raw_execute('UPDATE ttrss_user_entries t SET marked = not t.marked WHERE ref_id IN ('.implode(',', $toggle_marked).')');
		}
		ORM::get_db()->commit();

		return array($missing_feeds, $missing_entries, sizeof($toggle_unread), sizeof($toggle_marked));
	}

	function getUpdatedEntries($data) {
		if (is_null($data['last_sync'])) {
			return [];
		}
		$last_sync = date('Y-m-d H:i:s', $data['last_sync']);
		$entries = ORM::for_table('ttrss_entries')
				 ->select_many('link', 'ttrss_entries.title', 'updated', 'feed_url', 'marked', 'unread')
				 ->join('ttrss_user_entries', ['ttrss_entries.id', '=', 'ttrss_user_entries.ref_id'])
				 ->join('ttrss_feeds', ['ttrss_feeds.id', '=', 'ttrss_user_entries.feed_id'])
				 ->where_raw('ttrss_user_entries.last_read >= ? OR ttrss_user_entries.last_marked >= ?', [$last_sync, $last_sync])
				 ->where('ttrss_user_entries.owner_uid', $_SESSION['uid'])
				 ->find_array();
		return $entries;
	}

	function syncElfeed() {
		$data = $_REQUEST["data"];

		list($missing_feeds, $missing_entries, $toggled_unread, $toggled_marked) = $this->updateEntries($data);
		$changed_entries = $this->getUpdatedEntries($data);
		return array(API::STATUS_OK,
					 array(
						 'missing_feeds' => $missing_feeds,
						 'missing_entries' => $missing_entries,
						 'toggled_unread' => $toggled_unread,
						 'toggled_marked' => $toggled_marked,
						 'updated' => $changed_entries
					 )
		);
	}
}
