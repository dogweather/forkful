---
title:                "Arbeiten mit CSV"
html_title:           "PHP: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?

CSV steht für "Comma Separated Values" und ist ein einfaches Dateiformat zur Speicherung von tabellarischen Daten. Programmierer nutzen es häufig, um Daten zwischen verschiedenen Programmen oder Datenbanken zu importieren oder exportieren. Es ist auch nützlich, wenn man große Datenmengen schnell verarbeiten möchte.

## Wie geht das?

PHP bietet eine Vielzahl von Funktionen, um mit CSV-Dateien zu arbeiten. Zum Beispiel kann man mit der Funktion `fgetcsv()` eine Zeile aus einer CSV-Datei in ein Array umwandeln. Und mit `fputcsv()` kann man ein Array in eine Zeile in eine CSV-Datei schreiben. Hier ist ein Beispiel für das Einlesen und Schreiben von Daten in eine CSV-Datei:

```PHP
// Daten in Array speichern
$data = array(
    array('Name', 'Alter', 'Stadt'),
    array('Peter', 25, 'Berlin'),
    array('Maria', 30, 'Hamburg'),
    array('Max', 40, 'München'),
);

// CSV-Datei öffnen und Daten schreiben
$handle = fopen('daten.csv', 'w');
foreach ($data as $row) {
    fputcsv($handle, $row);
}
fclose($handle);

// CSV-Datei öffnen und Daten auslesen
$handle = fopen('daten.csv', 'r');
while (($row = fgetcsv($handle)) !== false) {
    // Daten in Array ausgeben
    print_r($row);
}
fclose($handle);

// Output:
// Array
// (
//     [0] => Name
//     [1] => Alter
//     [2] => Stadt
// )
// Array
// (
//     [0] => Peter
//     [1] => 25
//     [2] => Berlin
// )
// Array
// (
//     [0] => Maria
//     [1] => 30
//     [2] => Hamburg
// )
// Array
// (
//     [0] => Max
//     [1] => 40
//     [2] => München
// )
```

## Tiefseetauchen

CSV wurde in den 1970er Jahren entwickelt und hat sich seitdem zu einem der am häufigsten genutzten Dateiformate entwickelt. Alternativen wie XML oder JSON sind ebenfalls weit verbreitet, aber CSV bleibt aufgrund seiner Einfachheit und Lesbarkeit nach wie vor beliebt. Beim Importieren oder Exportieren von CSV-Dateien muss man möglicherweise auf Besonderheiten wie Zeichensatz oder Trennzeichen achten, um sicherzustellen, dass die Daten korrekt verarbeitet werden.

## Siehe auch

- [CSV-Bibliothek von PHP League](https://csv.thephpleague.com/)
- [Wikipedia-Artikel über CSV](https://de.wikipedia.org/wiki/Comma-separated_values)