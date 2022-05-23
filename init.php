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

		$this->host->add_api_method("kek", $this);
	}

	/**
	 * Our own API.
	 */
	function kek() {
		return array(API::STATUS_OK, array("ok" => true));
	}
}
