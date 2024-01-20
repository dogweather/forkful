---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# PHP: Datum aus einem String parsen

## Was & Warum?
Das Parsen eines Datums aus einem String ermöglicht ein einfaches Lesen und Manipulieren von Datums- und Zeitinformationen, die als Text vorliegen. Es ist ein wichtiger Vorgang, der hilft, Benutzereingaben, Datendateien oder Netzwerknachrichten zu analysieren.

## So geht's:
In PHP ermöglicht die eingebaute Funktion `date_parse` das Parsen eines Datums aus einem String.

```PHP
$string = "2022-07-05";
$Datum = date_parse($string);
print_r($Datum);
```

Die obige Code gibt die folgende Ausgabe:

```PHP
Array
(
    [year] => 2022
    [month] => 7
    [day] => 5
    [hour] => 
    [minute] => 
    [second] => 
    [fraction] => 
    [warning_count] => 0
    [warnings] => Array
        (
        )

    [error_count] => 0
    [errors] => Array
        (
        )

    [is_localtime] => 
)
```

## Tiefgang
Die Bereitstellung der `date_parse`-Funktion in PHP ist keineswegs selbstverständlich. In früheren versionen musste das Parsen manuell erfolgen.

Alternative Funktionen wie `DateTime::createFromFormat` ermöglichen weitere Flexibilität, indem sie das Format des Datumsstrings bestimmen.

Die `date_parse` Funktion selbst verwendet ein internes Datum- und Zeit-Modell, um das geparste Datum korrekt zu repräsentieren.

## Siehe auch
Um mehr über eingebaute Datum- und Zeitfunktionen in PHP zu erfahren, beachten Sie bitte die offizielle PHP-Dokumentation [hier](https://www.php.net/manual/de/book.datetime.php).
Für eine tiefergehende Untersuchung der `DateTime::createFromFormat` Funktion, klicken Sie [hier](https://www.php.net/manual/de/datetime.createfromformat.php).
Um mehr über Programmierpraktiken und -techniken in PHP zu lernen, besuchen Sie die PHP-Tutorial-Seite [hier](https://www.php.net/tut.php).