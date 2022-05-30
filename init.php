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
		$this->host->add_api_method("getSyncEntries", $this);
		$this->host->add_api_method("toggleEntries", $this);
		$this->host->add_api_method("readOldEntries", $this);
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

	function getSyncEntries() {
		$data = $_REQUEST["data"];
		$entries_query = ORM::for_table('ttrss_entries')
					   ->table_alias('e')
					   ->select_many('e.id', 'e.link', 'e.title', 'e.updated',
									 'ue.marked', 'ue.unread', 'f.feed_url', 'ue.last_read', 'ue.last_marked')
					   ->join('ttrss_user_entries', array('e.id', '=', 'ue.ref_id'), 'ue')
					   ->join('ttrss_feeds', array('ue.feed_id', '=', 'f.id'), 'f')
					   ->where('ue.owner_uid', $_SESSION['uid']);
		if (!is_null($data['look_back'])) {
			$entries_query = $entries_query->where_gte('updated', date('Y-m-d H:i:s', time() - $data['look_back']));
		}
		$entries = $entries_query->find_array();

		$bad_feed_links = array();
		foreach($data['bad_feeds'] as $bad_feed_link) {
			$bad_feed_links[$bad_feed_link] = true;
		}

		$result = array();
		foreach ($entries as $entry) {
			if (array_key_exists($entry['feed_url'], $bad_feed_links)) {
				$result[] = array(
					'id' => $entry['id'],
					'link' => $entry['link'],
					'title' => $entry['title'],
					'updated' => $entry['updated'] ? strtotime($entry['updated']) : null,
					'marked' => $entry['marked'],
					'unread' => $entry['unread'],
					'feed_url' => $entry['feed_url'],
					'last_read' => $entry['last_read'] ? strtotime($entry['last_read']) : null,
					'last_marked' => $entry['last_marked'] ? strtotime($entry['last_marked']) : null
				);
			} else {
				$result[] = array(
					'id' => $entry['id'],
					'link' => $entry['link'],
					'marked' => $entry['marked'],
					'unread' => $entry['unread'],
					'feed_url' => $entry['feed_url'],
					'last_read' => $entry['last_read'] ? strtotime($entry['last_read']) : null,
					'last_marked' => $entry['last_marked'] ? strtotime($entry['last_marked']) : null
				);
			}
		}

		return array(API::STATUS_OK, array('entries' => $result));
	}

	function toggleEntries() {
		$data = $_REQUEST["data"];
		$toggle_unread = $data["toggle_unread"];
		$toggle_marked = $data["toggle_marked"];
		ORM::get_db()->beginTransaction();
		if (!is_null($toggle_unread) && count($toggle_unread) > 0) {
			ORM::raw_execute('UPDATE ttrss_user_entries t SET unread = not t.unread, last_read = NOW() WHERE ref_id IN ('.implode(',', $toggle_unread).')');
		}
		if (!is_null($toggle_marked) && count($toggle_marked) > 0) {
			ORM::raw_execute('UPDATE ttrss_user_entries t SET marked = not t.marked, last_marked = NOW() WHERE ref_id IN ('.implode(',', $toggle_marked).')');
		}
		ORM::get_db()->commit();

		return array(API::STATUS_OK, array());
	}

	function readOldEntries() {
		$data = $_REQUEST["data"];
		$date = date('Y-m-d H:i:s', time() - $data['look_back']);
		$query=<<<EOD
UPDATE ttrss_user_entries t
SET unread = false
FROM ttrss_entries e
WHERE t.owner_uid = ? AND e.updated < ? AND t.ref_id = e.id
EOD;
		ORM::raw_execute($query, array($_SESSION['uid'], $date));
		return array(API::STATUS_OK, array());
	}
}
