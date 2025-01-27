<?php
session_start();

// Bedingungen definieren (z. B. über Formular oder feste Werte)
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $user_type = $_POST['user_type']; // Daten aus einem Formular
    $_SESSION['user_type'] = $user_type; // Speichern der Bedingung in der Session

    // Weiterleiten basierend auf der Bedingung
    if ($user_type === 'con1') {
        header('Location: testUmfrage.html');
    } elseif ($user_type === 'con2') {
        header('Location: testChatbase.html');
    } 
    exit;
}
?>

<html>
<head></head>
<body>
    <form method="POST">
        <label for="user_type">Wähle eine Bedingung</label>
        <select name="user_type" id="user_type">
            <option value="con1">Kondition 1 - Umfrageseite</option>
            <option value="con2">Kondition 2 - Chatbase</option>
        </select>
        <button type="submit">Weiter</button>
    </form>
</body>
</html>