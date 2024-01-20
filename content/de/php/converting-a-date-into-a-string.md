---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Umwandlung eines Datums in eine Zeichenkette (String) bezeichnet den Prozess, bei dem ein Datum in eine lesbare Textform konvertiert wird. Dies ermöglicht es Programmierern, Datumsinformationen in einer für Menschen lesbaren Form zu speichern und anzuzeigen.

## So geht's:
Zum Konvertieren eines Datums in einen String in PHP verwenden wir die eingebaute Funktion `date()`. Hier ist ein einfacher Code:

```PHP
<?php
$heute = date("d.m.Y");
echo "Heute ist " . $heute;
?>
```
Dieser Code gibt Folgendes aus:
```PHP
Heute ist 21.02.2022
```
'Heute' ist nun eine Zeichenkette (String), die das aktuelle Datum im Format TT.MM.JJJJ darstellt.

## Vertiefung
Die `date()` Funktion in PHP hat eine lange Geschichte und ist seit PHP Version 5.1.0 vorhanden. Es gibt auch Alternativen wie `DateTime::format()`, die mehr Flexibilität und Funktionalität bieten, aber die `date()` Funktion reicht für die meisten grundlegenden Anforderungen aus.

Die Implementierung im Hintergrund von `date()` wandelt ein Unix-Timestamp (Sekunden seit der Unix-Ära, die am 1. Januar 1970 begann) in ein menschenlesbares Datum um. Sie akzeptiert einen Formatparameter, der bestimmt, in welcher Form das Datum dargestellt werden soll, und einen optionalen Timestamp-Parameter. Wenn kein Timestamp angegeben wird, wird das aktuelle Datum und die aktuelle Zeit verwendet.

## Siehe auch
Für weiterführende Informationen, siehe die offiziellen PHP-Dokumentation:
- `date()` Funktion: https://www.php.net/manual/de/function.date.php 
- `DateTime::format()` Funktion: https://www.php.net/manual/de/datetime.format.php 
- UNIX Timestamp: https://www.php.net/manual/de/datetime.construct.php