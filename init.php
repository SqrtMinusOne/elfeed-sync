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
			$rc = Feeds::_subscribe($feed_datum["url"], $cat_id);
			if ($rc["code"] === 0 || $rc["code"] === 1) {
				$feed = ORM::for_table('ttrss_feeds')
					->where('id', $rc["feed_id"])
					->where('owner_uid', $_SESSION['uid'])
					->find_one();
				$feed->set([
					'title' => $feed_datum['title'],
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

	/**
	 * Our own API.
	 */
	function setFeedsTree() {
		$tree = $_REQUEST["tree"];

		list($categories, $feeds) = $this->createCategoriesTree($tree);
		$subscribed = $this->mergeCategoriesTree($categories, $feeds);

		return array(API::STATUS_OK, array('subscribed' => $subscribed));
	}
}
