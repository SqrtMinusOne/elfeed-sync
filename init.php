<?php
class Elfeed_Sync extends Plugin {
	private $host;

	function about() {
		return array(1.0, "Elfeed Sync", "Sync API for elfeed", false, "http://example.com");
	}

	function api_version() {
		return 1;
	}
	function init($host) {
		$this->host = $host;
	}

	function testMethod() {
		return array(API::STATUS_OK, array("ok" => true));
	}
}
