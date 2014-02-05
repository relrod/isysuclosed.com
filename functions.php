<?php

class Closings {
  protected $closings_src;
  protected $apikey;
  protected $alert_src;
  protected $wx_src;
  protected $alerts;
  protected $wx;

  function __construct() {
    date_default_timezone_set('America/New_York');
    $this->closings_src = file_get_contents('http://wkbn.com/closings/');
    $this->apikey = trim(file_get_contents('wunderground_api_key'));

    if (!file_exists('alerts.json') || (filemtime('alerts.json') < (time() - 1800))) {
      $this->alert_src = file_get_contents(
        'http://api.wunderground.com/api/'.$this->apikey.
        '/alerts/q/OH/Youngstown.json');
      file_put_contents('alerts.json', $this->alert_src);
    } else {
      $this->alert_src = file_get_contents('alerts.json');
    }

    if (!file_exists('wx.json') || (filemtime('wx.json') < (time() - 1800))) {
      $this->wx_src = file_get_contents(
        'http://api.wunderground.com/api/'.$this->apikey.
        '/conditions/q/OH/Youngstown.json');
      file_put_contents('wx.json', $this->wx_src);
    } else {
      $this->wx_src = file_get_contents('wx.json');
    }

    $this->alerts = json_decode($this->alert_src, true);
    $this->wx = json_decode($this->wx_src, true);
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

  function weatherconds() {
    return $this->wx['current_observation'];
  }
}
