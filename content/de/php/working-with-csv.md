---
title:                "Arbeiten mit csv"
html_title:           "PHP: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV (Comma-Separated Values) ist ein gängiges Dateiformat für den Austausch von Daten. Es ist einfach zu erstellen und kann von vielen Programmen und Plattformen geöffnet werden. In diesem Artikel sehen wir uns an, wie man CSV-Dateien mithilfe von PHP behandelt und weiterverarbeitet.

## Wie geht das?

Die Verarbeitung von CSV-Dateien mit PHP ist relativ einfach. Zunächst müssen wir die CSV-Datei öffnen und die Daten auslesen. Dafür können wir die Funktion `fopen()` verwenden, die uns einen Dateizeiger zurückgibt. Dieser Zeiger wird dann an die Funktion `fgetcsv()` übergeben, welche die Daten in ein Array umwandelt.

```
<?php
// CSV-Datei öffnen
$handle = fopen("meine_datei.csv", "r");

// Daten auslesen und in ein Array umwandeln
while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
  $array[] = $data;
}

// Datei schließen
fclose($handle);

// Ausgabe des Arrays
print_r($array);
?>
```

Um die Daten weiter zu verarbeiten, können wir nun auf die einzelnen Werte im Array zugreifen. Zum Beispiel können wir eine Tabelle erstellen und die Daten in HTML ausgeben.

```
<table>
  <tr>
    <th>Name</th>
    <th>Alter</th>
    <th>Stadt</th>
  </tr>
  <?php
    for($i = 0; $i < count($array); $i++) {
      echo "<tr>";
      echo "<td>" . $array[$i][0] . "</td>";
      echo "<td>" . $array[$i][1] . "</td>";
      echo "<td>" . $array[$i][2] . "</td>";
      echo "</tr>";
    }
  ?>
</table>
```

Die Ausgabe könnte dann beispielsweise so aussehen:

| Name      | Alter | Stadt     |
|-----------|-------|-----------|
| Max Mustermann | 27    | Berlin    |
| Lisa Müller   | 32    | Köln    |
| Paul Schmidt | 42    | Hamburg |

## Tiefer eintauchen

PHP bietet auch die Möglichkeit, CSV-Dateien zu schreiben und zu bearbeiten. Dafür gibt es die Funktion `fputcsv()`, die ein Array in das CSV-Format umwandelt und in die Datei schreibt. Auch das Hinzufügen oder Löschen von Spalten oder Zeilen ist mit PHP möglich.

Es ist jedoch wichtig zu beachten, dass CSV-Dateien auch ihre Grenzen haben und nicht für alle Datentypen geeignet sind. Zum Beispiel können komplexere Datenstrukturen wie Arrays nicht direkt in eine CSV-Datei geschrieben werden. Hier müssen gegebenenfalls andere Datenformate verwendet werden.

## Siehe auch

- [PHP-Handbuch: Arbeiten mit CSV-Dateien](https://www.php.net/manual/de/function.fgetcsv.php)
- [Tutorial: CSV-Dateien mit PHP weiterverarbeiten](https://www.php-einfach.de/experte/php-tutorial/csv-datei-einlesen-und-ausgeben/)