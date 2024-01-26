---
title:                "Webseite herunterladen"
date:                  2024-01-20T17:44:57.086864-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Herunterladen einer Webseite bedeutet, ihre Inhalte zu holen, um sie zu analysieren oder zu verarbeiten. Programmierer tun das, um Daten zu sammeln, Automatisierungen zu erstellen oder den Inhalt offline verfügbar zu machen.

## How to: (Anleitung:)
```PHP
<?php
$url = "https://www.beispielwebseite.de";
$ch = curl_init($url); // Initialisiere cURL-Sitzung
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true); // Gibt das Ergebnis als String zurück

$webseite = curl_exec($ch); // Führe die cURL-Sitzung aus und speichere das Ergebnis

if ($webseite === false) {
    echo "Fehler: " . curl_error($ch);
} else {
    echo "Webseite heruntergeladen: \n";
    echo $webseite; // Zeigt den Inhalt der heruntergeladenen Seite an
}

curl_close($ch); // Beende die cURL-Sitzung
?>
```

Sample Output:
```
Webseite heruntergeladen:
<!DOCTYPE html>
<html lang="de">
<head>
    <title>Beispielwebseite</title>
...
</html>
```

## Deep Dive (Tiefgang):
Historisch gesehen haben sich viele PHP-Versionen an der Art und Weise orientiert, wie das Web verstanden und bearbeitet wurde. Die Verwendung von cURL in PHP ist eine populäre Methode, um mit Protokollen wie HTTP zu arbeiten. Alternativ könnten Programmierer auch `file_get_contents()` verwenden, wenn es nur um einfache GET-Anfragen geht und keine feinkörnige Kontrolle benötigt wird. Beim Herunterladen von Webseiten gibt es jedoch viele Implementierungsdetails zu beachten: Header-Management, Cookies, Redirects, SSL, Fehlerbehandlung und vieles mehr, was mit cURL oft besser zu handhaben ist.

cURL bietet umfangreiche Optionen zur Steuerung der HTTP-Anfragen und ist daher für komplexere Aufgaben geeignet. Es beherrscht auch andere Protokolle wie FTP, SMTP und so weiter. In der Praxis heißt das, dass du cURL fast für jede Form von Netzwerkkommunikation in deinen PHP-Anwendungen nutzen kannst.

## See Also (Siehe auch):
- [PHP cURL Manual](https://www.php.net/manual/de/book.curl.php): Offizielle PHP-Dokumentation für cURL-Funktionen.
- [HTTP Protokoll](https://developer.mozilla.org/de/docs/Web/HTTP): Eine detaillierte Beschreibung des HTTP-Protokolls.
- [file_get_contents() Funktion](https://www.php.net/manual/de/function.file-get-contents.php): Offizielle PHP-Dokumentation für den file_get_contents()-Befehl.
- [PHP-Sicherheitshinweise](https://www.php.net/manual/de/security.php): Sicherheitsaspekte beim Arbeiten mit PHP.
