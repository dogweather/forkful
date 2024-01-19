---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die aktuelle Datumsabfrage ist eine häufig genutzte Funktion in der Programmierung, die es uns ermöglicht, das aktuelle Datum und die Uhrzeit abzurufen. Programmierer nutzen diese Funktion in einer Vielzahl von Szenarien, wie zum Beispiel bei der Zeiterfassung, der Erstellung von Zeitstempeln oder der Durchführung von zeitkritischen Berechnungen.

## Wie geht das?

Hier ist ein einfacher Code, um das aktuelle Datum in PHP zu erhalten:

```PHP
<?php
$heute = date("Y-m-d H:i:s");
echo $heute;
?>
```

In diesem Code steht `Y-m-d H:i:s` für Jahr-Monat-Tag Stunde:Minute:Sekunde entsprechend. Wenn Sie den Code ausführen, erhalten Sie eine Ausgabe wie die folgende:

```PHP
2022-02-14 20:45:32
```

## In die Tiefe gehen

Die Art und Weise, wie wir in PHP das aktuelle Datum abrufen, hat sich im Laufe der Jahre nicht wesentlich verändert. Frühere Versionen verwendeten eine ähnliche `date` Funktion. Alternativ könnten Sie auch die Funktion `DateTime()` verwenden, die genauer und flexibler sein kann, besonders wenn es um Zeitzonen geht:

```PHP
<?php
$datum = new DateTime();
echo $datum->format('Y-m-d H:i:s');
?>
```
Bezüglich der Implementierungsdetails verwendet PHP intern die `time()` Funktion, um das aktuelle Unix Timestamp zu erhalten, das dann in das menschenlesbare Format konvertiert wird.

## Siehe auch

Für weitere Details über Datums- und Zeitfunktionen in PHP, schauen Sie sich diese Links an:

- PHP date() Funktion: https://www.php.net/manual/de/function.date.php
- PHP Zeit- und Datumsformatierung: https://www.php.net/manual/de/datetime.format.php