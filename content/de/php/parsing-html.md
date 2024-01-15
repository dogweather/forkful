---
title:                "HTML-Analyse"
html_title:           "PHP: HTML-Analyse"
simple_title:         "HTML-Analyse"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man HTML parsen? Nun, HTML ist die Sprache, in der Webseiten geschrieben werden, und um auf Informationen oder Inhalte einer Webseite zugreifen zu können, müssen wir sie zunächst verstehen. Das Parsen von HTML ermöglicht es uns, die Struktur und den Inhalt einer Webseite zu analysieren und zu verarbeiten, um die gewünschten Informationen zu erhalten.

## Wie Geht's

Das Parsen von HTML ist relativ einfach mit PHP, da es spezielle Funktionen gibt, die uns dabei unterstützen. Hier ist ein einfaches Beispiel, um den Titel einer Webseite auszulesen:

```PHP 
// HTML-Datei einlesen 
$html = file_get_contents('https://www.example.com'); 

// DOMDocument-Objekt erstellen 
$dom = new DOMDocument(); 

// HTML-Code in das Objekt laden 
$dom->loadHTML($html); 

// Das <title>-Tag finden und den Text ausgeben 
$title = $dom->getElementsByTagName('title')->item(0)->textContent; 

echo $title; // Ausgabe: Beispiel Webseite 
```

In diesem Beispiel haben wir zuerst die HTML-Datei mit der Funktion "file_get_contents" eingelesen und anschließend ein DOMDocument-Objekt erstellt. Mit der "loadHTML"-Funktion haben wir den HTML-Code in das Objekt geladen und konnten dann mithilfe von "getElementsByTagName" das <title>-Tag finden und den Text ausgeben.

Es gibt viele weitere Funktionen, die uns beim Parsen von HTML helfen, wie zum Beispiel "getElementById", "getElementsByTagName", "getAttribute" usw. Mit diesen Funktionen können wir auf bestimmte Elemente, Attribute und Inhalte in einem HTML-Dokument zugreifen und sie verarbeiten.

## Tieferer Einblick

Das Parsen von HTML ermöglicht es uns nicht nur, Daten aus einer Webseite zu extrahieren, sondern auch sie zu manipulieren und neu zu erstellen. Wir können zum Beispiel mit der "createElement" Funktion neue HTML-Elemente erstellen und sie dann in das bestehende HTML-Dokument einfügen.

Außerdem können wir mit dem Einsatz von Funktionen wie "strip_tags" oder "htmlspecialchars" unerwünschte oder gefährliche Elemente und Zeichen aus dem HTML-Code filtern, was die Sicherheit unserer Anwendung verbessert.

Es gibt auch fortgeschrittene Techniken wie das Einbinden von XPATH-Ausdrücken, um gezielt auf bestimmte Elemente eines HTML-Dokuments zuzugreifen. Für einen tieferen Einblick in das Parsen von HTML empfehle ich, sich mit den verschiedenen Funktionen und Konzepten der PHP-DOM-Erweiterung vertraut zu machen.

## Siehe Auch

- [PHP Manual: DOMDocument](https://www.php.net/manual/de/book.dom.php)
- [W3Schools: PHP DOMDocument](https://www.w3schools.com/php/php_ref_dom.asp)
- [WebScraping.com: PHP und HTML Parsing](https://www.webscraping.com/de/blog/PHP-und-HTML-Parsing/)