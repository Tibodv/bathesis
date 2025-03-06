<?php
// Randomly assign a value of 0 or 1
$randomChoice = random_int(0, 1);

if ($randomChoice === 0){
    header('Location: mockupCb.html');
}
elseif ($randomChoice === 1){
    header('Location: mockupNoCb.html');

}
exit;

?>
<!DOCTYPE html>
<html lang="de">
<head>
  <meta charset="UTF-8">
  <title>Zufällige Zuweisung</title>
</head>
<body>
  <h1>Zufälliges Ergebnis</h1>
  <?php
  if ($randomChoice === 0) {
      echo '<p>Sie wurden der Gruppe A zugewiesen.</p>';
  } else {
      echo '<p>Sie wurden der Gruppe B zugewiesen.</p>';
  }
  ?>
</body>
</html>
