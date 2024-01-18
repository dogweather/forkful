---
title:                "Das Parsen eines Datums aus einem String"
html_title:           "Ruby: Das Parsen eines Datums aus einem String"
simple_title:         "Das Parsen eines Datums aus einem String"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Parsen eines Datums aus einer Zeichenfolge geht es darum, ein Datum in einer lesbaren Form aus einem Textformat zu extrahieren. Programmierer nutzen dies, um Daten aus unterschiedlichen Quellen zu verarbeiten und in einem einheitlichen Format zu präsentieren.

## So geht's:
Wenn du ein Datum aus einer Zeichenfolge parsen möchtest, kannst du einfach das ```Date```-Modul in Ruby nutzen. Zum Beispiel:
```
require 'date'
Date.parse("25.08.2021") # Output => 2021-08-25
```

## Tiefgehende Einblicke:
Das Parsen von Datumsangaben aus einer Zeichenfolge ist seit langer Zeit eine häufige Aufgabe für Programmierer. Früher mussten sie diese Funktion oft von Grund auf neu programmieren, aber heute bieten die meisten Programmiersprachen und Frameworks eingebaute Methoden dazu an. Eine Alternative zu Ruby ist zum Beispiel das Moment.js-Framework in JavaScript. Bei der Implementierung des Parsings von Datumsangaben ist es wichtig, auf die Formatierung der Zeichenfolge und mögliche Fehlerquellen wie falsche Eingaben zu achten.

## Siehe auch:
- [Ruby Date Dokumentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Moment.js Dokumentation](https://momentjs.com/docs/#/parsing/)