---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-Separated Values" und ist ein Dateiformat, das Daten in Tabellenform speichert. Programmierer nutzen CSV, weil es leicht verständlich, einfach zu lesen und zu schreiben ist und von den meisten Tabellenkalkulationsprogrammen unterstützt wird.

## Anleitung
Lese eine CSV-Datei:
```php
<?php
$handle = fopen("meine_daten.csv", "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        print_r($data);
    }
    fclose($handle);
}
?>
```
Schreibe eine CSV-Datei:
```php
<?php
$list = array (
    array('Alice', 'Bob', 'Charlie'),
    array('Deutschland', 'Frankreich', 'Italien'),
);

$fp = fopen('datei.csv', 'w');

foreach ($list as $fields) {
    fputcsv($fp, $fields);
}

fclose($fp);
?>
```
Ausgabe für das Lesebeispiel (abhängig von `meine_daten.csv`):
```
Array
(
    [0] => Alice
    [1] => Bob
    [2] => Charlie
)
...
```

## Deep Dive
CSV-Dateien sind seit den frühen Anfängen der Computertechnologie im Einsatz und erlauben eine einfache Interoperabilität zwischen verschiedenen Programmen. Neben Standard-CSV gibt es Formate wie TSV (Tab-Separated Values) - wobei die Werte durch Tabulatoren getrennt sind. In PHP arbeitet man häufig mit den eingebauten Funktionen `fgetcsv()` und `fputcsv()`, die komfortabel Zeilen lesen und schreiben. Dabei ist auf korrektes Encoding und die Einhaltung des gewünschten Trennzeichens zu achten.

## See Also
- PHP Manual auf CSV-Funktionen: https://www.php.net/manual/de/function.fgetcsv.php
- RFC 4180, das den CSV-Standard beschreibt: https://tools.ietf.org/html/rfc4180
- Eine Diskussion über CSV-Manipulation in PHP auf Stack Overflow: https://stackoverflow.com/questions/tagged/php+csv
