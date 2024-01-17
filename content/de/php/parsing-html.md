---
title:                "HTML-Parsing"
html_title:           "PHP: HTML-Parsing"
simple_title:         "HTML-Parsing"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML ist eine Technik, mit der Programmierer HTML-Code analysieren und extrahieren können. Dadurch können sie bestimmte Informationen aus einer Webseite isolieren und in ihren eigenen Programmen verwenden. Das Parsen von HTML wird häufig verwendet, um automatisierte Bots zu erstellen, die Daten von Websites sammeln oder um Inhalte auf Websites dynamisch zu aktualisieren.

## Wie geht's?
Eine einfache Möglichkeit, HTML mit PHP zu parsen, ist die Verwendung der `simple_html_dom`-Bibliothek. Diese Bibliothek bietet verschiedene Funktionen zum Durchsuchen und Extrahieren von HTML-Elementen. Zum Beispiel können Sie mit dem folgenden Code ein HTML-Dokument laden und alle Links auf der Seite auflisten:

```PHP
<?php
include 'simple_html_dom.php';

// HTML-Dokument laden
$html = file_get_html('https://www.example.com');

// Alle Links auswählen und ausgeben
foreach($html->find('a') as $link) {
  echo $link->href . '<br>';
}
?>
```

Die Ausgabe könnte beispielsweise so aussehen:
```
https://www.example.com/link1
https://www.example.com/link2
https://www.example.com/link3
```

## Tiefer eintauchen
Der erste Parser für HTML wurde bereits im Jahr 1980 entwickelt und seitdem gibt es viele verschiedene Parsing-Methoden und Bibliotheken. Neben der Verwendung von PHP gibt es auch andere Möglichkeiten, um HTML zu parsen, wie z.B. mit regulären Ausdrücken oder mit JavaScript. Jede Methode hat ihre eigenen Vor- und Nachteile, aber PHP ist eine häufig verwendete und bewährte Option.

Wenn Sie sich für die Verwendung von `simple_html_dom` entscheiden, gibt es viele weitere Funktionen, die Sie erkunden können. Zum Beispiel können Sie spezifische HTML-Tags oder -Attribute auswählen oder sogar auf CSS-Selektoren zugreifen.

## Siehe auch
- Offizielle Dokumentation für `simple_html_dom`: https://simplehtmldom.sourceforge.io/
- Einführung in das Parsen von HTML mit PHP: https://code.tutsplus.com/tutorials/html-parsing-and-screen-scraping-with-the-simple-html-dom-library--net-11856
- Alternative Methode zum Parsen von HTML mit PHP: https://github.com/azettl/php-html-parser