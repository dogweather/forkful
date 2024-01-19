---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Löschen von Zeichen, die mit einem Muster übereinstimmen, ist ein Kernaufgabe in der Textverarbeitung. Hierbei entfernt der Programmierer nützliche Daten aus großen Textmengen oder bereinigt Eingabe-Strings, die vom Nutzer kommen könnten.

## Wie zu:

Wir können die eingebaute PHP-Funktion `preg_replace()` verwenden, um Zeichenfolgen zu löschen, die einem bestimmten Muster entsprechen.

```PHP
<?php
$eingabe = "Das ist 1 Beispiel!";
$sauber = preg_replace("/[^A-Za-zäöüÄÖÜß ]/", "", $eingabe);
echo $sauber;
// Ausgabe: "Das ist Beispiel!"
?>
```

In diesem Fall entfernt `preg_replace()` alle Zeichen, die keine Buchstaben (einschließlich Umlaute) oder Leerraum sind.

## Tiefen-Tauchgang

Ursprünglich entwickelten Computerwissenschaftler reguläre Ausdrücke (RegEx), um Muster in Texten zu erkennen und zu manipulieren. Die `preg_replace()`-Funktion in PHP ist nur eine von vielen Möglichkeiten, reguläre Ausdrücke zu verwenden.

Alternativ könnten Sie auch `str_replace()` oder `strtr()` verwenden, aber diese Funktionen erlauben nur einfache ersetzen und nicht die Leistungsfähigkeit von Mustern wie `preg_replace()`.

Die genaue Funktionsweise von `preg_replace()` hängt vom verwendeten Muster ab. Die Funktion verwendet das PCRE (Perl Compatible Regular Expressions)-Format und es gibt zahlreiche Optionen und Variationen, mit denen Sie verschiedene Arten von Mustern definieren können.

## Siehe auch

Um mehr über die `preg_replace()`-Funktion zu erfahren, besuchen Sie die offizielle PHP-Dokumentation: [https://www.php.net/manual/de/function.preg-replace.php](https://www.php.net/manual/de/function.preg-replace.php)

Um mehr über die Geschichte und Theorie von regulären Ausdrücken zu erfahren, siehe [https://en.wikipedia.org/wiki/Regular_expression](https://en.wikipedia.org/wiki/Regular_expression).

Um den Unterschied zwischen `preg_replace()`, `str_replace()` und `strtr()` zu sehen, besuchen Sie [https://www.php.net/manual/de/function.str-replace.php](https://www.php.net/manual/de/function.str-replace.php) und [https://www.php.net/manual/de/function.strtr.php](https://www.php.net/manual/de/function.strtr.php).