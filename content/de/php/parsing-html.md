---
title:                "PHP: Das Parsen von HTML"
simple_title:         "Das Parsen von HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-html.md"
---

{{< edit_this_page >}}

## Warum HTML analysieren?

Parsing HTML ist eine wichtige Fähigkeit für jeden PHP-Programmierer. Es ermöglicht uns, wichtige Informationen aus einer Webseite auszulesen und automatisierte Aufgaben auszuführen, wie beispielsweise das Scraping von Daten oder das Erstellen von Web-Crawlern.

## Wie man HTML analysiert

Um HTML mit PHP zu analysieren, müssen wir zuerst die gewünschte Webseite herunterladen und in einer Variablen speichern. Dann können wir mit der Funktion `file_get_contents()` den gesamten HTML-Code in einer Zeichenkette erhalten. Anschließend können wir mit Hilfe von Regulären Ausdrücken oder der `DOMDocument`-Klasse gezielt nach bestimmten Elementen oder Inhalten suchen.

```PHP
<?php

// Webseite herunterladen
$html = file_get_contents('https://www.example.com');

// Regulärer Ausdruck um den Titel der Seite zu finden
preg_match("/<title>(.*?)<\/title>/", $html, $title);

// Ausgabe des Titels
echo $title[1];
```

Dieses Beispiel zeigt, wie wir mit Hilfe von Regulären Ausdrücken den Titel einer Webseite auslesen können. Natürlich gibt es noch viele weitere Möglichkeiten und Techniken, um HTML mit PHP zu analysieren.

## Tiefergehende Informationen über das Analysieren von HTML

Beim Analysieren von HTML ist es wichtig, die Struktur der Webseite zu verstehen und sich mit den verschiedenen verfügbaren Methoden vertraut zu machen. Die `DOMDocument`-Klasse bietet eine Vielzahl von Funktionen, um HTML-Dokumente zu durchsuchen und zu bearbeiten. Es ist auch empfehlenswert, sich mit den Grundlagen von Regulären Ausdrücken auseinanderzusetzen, um komplexe Suchmuster zu erstellen.

## Siehe auch

- [PHP: file_get_contents()](https://www.php.net/manual/de/function.file-get-contents.php)
- [PHP: DOMDocument](https://www.php.net/manual/de/class.domdocument.php)
- [Reguläre Ausdrücke- Einführung und Grundlagen](https://www.php.net/manual/de/book.pcre.php)