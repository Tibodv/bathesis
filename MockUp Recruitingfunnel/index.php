<?php
// Randomly assign a value of 0 or 1
$randomChoice = random_int(0, 1);


$filename = 'group_counts.json';

// Initialize the file if it doesn't exist
if (!file_exists($filename)) {
    file_put_contents($filename, json_encode(['groupA' => 0, 'groupB' => 0]));
}


// Open the file for reading and writing
$file = fopen($filename, 'r+');
if (flock($file, LOCK_EX)) { // acquire an exclusive lock
    // Read the current data
    $fileContents = file_get_contents($filename);
    $data = json_decode($fileContents, true);
    if (!is_array($data)) {
        $data = ['groupA' => 0, 'groupB' => 0];
    }
    // Update the appropriate counter
    if ($randomChoice === 0) {
        $data['groupA']++;  //Group A: CB
    } else {
        $data['groupB']++; //Group B: No CB
    }
    // Write the updated data back to the file
    ftruncate($file, 0);
    rewind($file);
    fwrite($file, json_encode($data));
    fflush($file);
    flock($file, LOCK_UN);
}
fclose($file);



if ($randomChoice === 0){
    header('Location: disclaimerCB.html');
}
elseif ($randomChoice === 1){
    header('Location: disclaimerNoCB.html');

}
exit;

?>
