---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein HTTP-Request ist ein Anforderungsprotokolldetail, das ein Client an einen Server sendet, um spezifische Informationen zu erhalten. Programmierer senden HTTP-Requests, um datengetriebene Operationen wie das Abrufen, Senden oder Aktualisieren von Daten auf einem Server durchzuführen.

## So geht's:

Mit PHP können Sie mithilfe der Funktion `file_get_contents` einen einfachen HTTP-GET-Request senden. Hier ist ein Beispiel:

```PHP
<?php
$response = file_get_contents('http://example.com');
echo $response;
?>
```
In diesem Code sendet `file_get_contents` einen HTTP-GET-Request an `http://example.com` und gibt die Antwort zurück. 

Für komplexere Anfragen können Sie die cURL-Bibliothek verwenden:

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, 'http://example.com');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

$response = curl_exec($ch);

curl_close ($ch);

echo $response;
?>
```
Dieser Code sendet ebenfalls einen HTTP-GET-Request an `http://example.com`, ermöglicht aber eine größere Kontrolle und Anpassungsmöglichkeiten.

## Vertiefung:

HTTP-Requests sind der Kern der Kommunikation im Web und wurden erstmals im HTTP/1.0-Standard von 1996 definiert. Neben GET gibt es noch eine Reihe anderer HTTP-Request-Methoden, darunter POST, PUT und DELETE, die verschiedene Arten von Interaktionen mit einem Server ermöglichen.

Es gibt viele Alternativen zu `file_get_contents` und cURL in PHP, einschließlich fsockopen, fopen und das Http-Paket von PECL. Welche Sie verwenden, hängt von Ihren spezifischen Bedürfnissen und Präferenzen ab. 

Die Implementierung von HTTP-Requests in PHP ist recht einfach, da die meisten Funktionen intern abstrahiert sind. Die Funktion `file_get_contents` beispielsweise verbirgt den tatsächlichen Prozess der Erstellung einer Socket-Verbindung, des Sendens eines HTTP-Requests und des Wartens auf die Antwort.

## Siehe auch:

1. PHP-Dokumentation: [file_get_contents](https://www.php.net/manual/de/function.file-get-contents.php) und [cURL](https://www.php.net/manual/de/book.curl.php)
2. W3Schools: [PHP Ajax Request](https://www.w3schools.com/php/php_ajax_php.asp)
3. [HTTP/1.0-Spezifikation](https://www.w3.org/Protocols/HTTP/1.0/spec.html)