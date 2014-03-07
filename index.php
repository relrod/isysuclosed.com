<?php
  require 'functions.php';
  $c = new Closings();
  $conds = $c->weatherconds();

  header('X-UA-Compatible: IE=edge');
  header('Cache-Control: no-cache');
?>
<!doctype html>
<html>
<head>
  <title>YSU Closing Status</title>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.1.0/css/bootstrap.min.css" />
  <script src="//cdnjs.cloudflare.com/ajax/libs/jquery/2.0.3/jquery.min.js"></script>
  <script src="//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.1.0/js/bootstrap.min.js"></script>
  <link href='http://fonts.googleapis.com/css?family=Open+Sans' rel='stylesheet' type='text/css'>
  <style>
    * { font-family: "Proxima Nova", "Open Sans" !important; }
    h1 { font-weight: bold; text-align: center; }
    p.t { font-size: 1.8em; text-align: center; }
  </style>
  <script>
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
    ga('create', 'UA-21458826-6', 'isysuclosed.com');
    ga('send', 'pageview');
  </script>
</head>
<body>
  <div class="container">
    <h1>YSU Closing Status</h1>

    <p class="t">So, here's the deal.</p>

    <p class="t">
    The weather is currently
    <strong><?php echo $conds['weather']; ?></strong> and
    <strong><?php echo $conds['temp_f']; ?>°F</strong>.
    </p>

    <p class="t">
    There are currently
    <strong><?php echo $c->closingcount(); ?></strong> delays/closings
    according to a local (Youngstown) news source.
    </p>

    <p class="t">
    Youngstown State University <strong>
    <?php
      if ($c->youngstownMentioned()) {
    ?>
        <span style="color: green;">WAS mentioned</span>
    <?php
      } else {
    ?>
        <span style="color: red;">was NOT mentioned</span>
    <?php
      }
    ?>
    </strong> among them.
    </p>

    <p class="t">
    There are currently <strong><?php echo $c->alertcount(); ?></strong> weather
    alert(s) covering Youngstown as of <strong>
    <?php echo $c->alertstime(); ?></strong>.
    </p>

    <ul>
      <?php
        foreach ($c->weatheralerts() as $alert) {
      ?>
      <li>
        <strong><?php echo $alert['description'] ?></strong>
        expiring <?php echo $alert['expires']; ?>
      </li>
      <?php
        }
      ?>
    </ul>

    <hr />

    <p style="text-align: center;"><small>
    This website is not affiliated with Youngstown State University in
    any way, and was written to make a point.
    </small></p>

    <p style="text-align: center;"><small>
    While hopefully accurate, this is NOT an official resource. Always confirm
    with
    <a href="https://swww.ysu.edu/downloads/closing_procedure.pdf">official</a>
    resources.
    </small></p>

    <p style="text-align: center; color: #888888;"><small>
    Valid HTML5 · Weather information by Weather Underground
    </small></p>

    <img
      style="display: block; margin: 0 auto; width: 180px;"
      src="http://icons.wxug.com/logos/images/wundergroundLogo_4c_horz.jpg"
      alt="Weather Underground Logo" />
  </div>
</body>
</html>
