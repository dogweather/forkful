---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite beinhaltet, den Inhalt einer Webseite als Daten zu speichern. Dies geschieht in der Regel, um die Daten zu analysieren, zu manipulieren oder offline zu speichern.

## Wie:

Hier ist ein einfacher PHP-Code zum Herunterladen von Webseiten:

```PHP
<?php
    $url="https://exemplarwebseite.de";
    $inhalt = file_get_contents($url);
    echo $inhalt;
?>
```
Dieser Code wird die HTML-Inhalte der Webseite 'https://exemplarwebseite.de' in der Konsole ausgeben.

## Vertiefung 

Historisch gesehen, hat das Herunterladen von Webseiten ihren Ursprung im Bedarf, offline auf Daten zuzugreifen, oder diese zu analysieren. Im Laufe der Zeit entwickelten sich jedoch mehr Möglichkeiten wie z.B. das Web-Scraping.

Es gibt Alternativen zu PHP für dieses Aufgabe. Python bietet z.B. mit seinen Bibliotheken 'requests' und 'beautiful soup' ausgezeichnete Optionen. 

Mit PHP wird der Inhalt mit der Funktion `file_get_contents()` heruntergeladen. Diese Funktion liest eine Datei in einen String. Bei Anwendung auf eine URL liest es den Inhalt der Webseite in einen String.

## Siehe auch:

1. [PHP: file_get_contents - Manual](https://www.php.net/manual/de/function.file-get-contents.php)