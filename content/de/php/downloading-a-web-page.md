---
title:                "Herunterladen einer Webseite"
html_title:           "PHP: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Lassen Sie uns ehrlich sein, wir alle lieben das Internet. Egal, ob wir nach Informationen suchen, Unterhaltung genießen oder mit Freunden und Familie in Verbindung bleiben wollen, das Web bietet uns endlose Möglichkeiten. Wenn Sie also eine Webseite herunterladen, können Sie sie jederzeit und überall offline lesen, ohne auf eine Internetverbindung angewiesen zu sein. Genial, oder?

## Wie man eine Webseite herunterlädt

Das Herunterladen einer Webseite mit PHP ist ziemlich einfach und erfordert nur wenige Zeilen Code. Zunächst müssen Sie die URL der Webseite angeben, die Sie herunterladen möchten. Dann verwenden Sie die Funktion `file_get_contents()`, um den Inhalt der Webseite als Zeichenkette zu erhalten. Schließlich können Sie den Inhalt auf Ihrer eigenen Seite ausgeben oder in einer Datei speichern.

```PHP
$url = "https://example.com";
$webpage = file_get_contents($url);
echo $webpage; // Gibt den Inhalt der Webseite aus
file_put_contents("webseite.html", $webpage); // Speichert den Inhalt in einer Datei
```

Die Funktion `file_get_contents()` kann auch verwendet werden, um Daten von APIs oder anderen Online-Quellen abzurufen. Dies macht sie zu einem nützlichen Werkzeug für die Entwicklung von Webanwendungen.

## Deep Dive

Wenn Sie sich für die technischen Details interessieren, verwendet die Funktion `file_get_contents()` die HTTP-Stream-Wrapper, um die angegebene Webseite abzurufen und deren Inhalt zurückzugeben. Dieser Inhalt wird als Zeichenkette mit den Header-Informationen der Webseite zurückgegeben. Sie können auch die Funktion `get_headers()` verwenden, um nur die Header-Informationen zu erhalten.

Eine andere Möglichkeit, eine Webseite herunterzuladen, ist die Verwendung der cURL-Bibliothek. Sie bietet mehr Flexibilität und Kontrolle über den Herunterladeprozess, ist aber auch etwas komplexer. Ein Beispiel dafür würde den Rahmen dieses Artikels sprengen, aber Sie können im "See Also" Abschnitt nach Links zur cURL-Dokumentation suchen.

## Siehe auch

- PHP `file_get_contents()` Dokumentation: https://www.php.net/manual/de/function.file-get-contents.php
- PHP `get_headers()` Dokumentation: https://www.php.net/manual/de/function.get-headers.php
- cURL Dokumentation: https://www.php.net/manual/de/book.curl.php