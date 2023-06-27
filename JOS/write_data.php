<?php
// get the data from the POST message
$post_data = json_decode(file_get_contents('php://input'), true);
$data = $post_data['filedata'];

// Select only columns to yoke
//$data = $data->rt

// the directory "data" must be writable by the server
$name = "Data/most_recent.csv";
// write the file to disk
file_put_contents($name, $data);
?>
