<?php

class Closings {
  protected $closings_src;
  protected $alert_src;
  protected $alerts;

  function __construct() {
    date_default_timezone_set('America/New_York');
    $this->closings_src = file_get_contents('http://wkbn.com/closings/');
    if (!file_exists('alerts.json') || (filemtime('alerts.json') < (time() - 1800))) {
      $apikey = file_get_contents('wunderground_api_key');
      $this->alert_src = file_get_contents(
        'http://api.wunderground.com/api/'.$apikey.
        '/alerts/q/OH/Youngstown.json');
      file_put_contents('alerts.json', $this->alert_src);
    } else {
      $this->alert_src = file_get_contents('alerts.json');
    }
    $this->alerts = json_decode($this->alert_src, true);
  }

  function alertstime() {
    $d = filemtime('alerts.json');
    return date('r', $d);
  }

  function closingcount() {
    preg_match('@(\d+) Closings &amp; Delays@', $this->closings_src, $matches);
    if (count($matches) > 0) {
      return $matches[1];
    } else {
      return 0;
    }
  }

  function youngstownMentioned() {
    return preg_match(
      '@Youngstown State University@i',
      $this->closings_src,
      $matches);
  }

  function alertcount() {
    return count($this->alerts['alerts']);
  }

  function weatheralerts() {
    return $this->alerts['alerts'];
  }
}
