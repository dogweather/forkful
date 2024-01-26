---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:37:51.692723-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Parsing bedeutet, einen Datumswert aus einem String herauszulesen. Programmierer machen das, um Benutzerdaten zu verarbeiten, Termine zu speichern oder Zeiträume zu berechnen.

## So geht's:
In PHP nutzen wir das `DateTime` Objekt für das Parsing von Datumsstrings. Mit der `createFromFormat` Methode können wir Formate definieren und umwandeln.

```PHP
<?php
$datumString = "2023-04-01 14:30:00";

// Datum aus einem definierten Format erstellen
$datumObjekt = DateTime::createFromFormat('Y-m-d H:i:s', $datumString);

// Ausgabe des DateTime-Objekts
echo $datumObjekt->format('Y-m-d H:i:s');
?>
```

Ausgabewert wäre: 
```
2023-04-01 14:30:00
```

Mit `strtotime()` können wir die Erstellung des `DateTime` Objekts auch vereinfachen, allerdings ohne ein festes Format zu definieren:

```PHP
<?php
$datumString = "next Thursday";

// Datum parsen und ausgeben
$datum = strtotime($datumString);
echo date('Y-m-d H:i:s', $datum);
?>
```

Ausgabewert könnte sein (abhängig vom aktuellen Datum):
```
2023-04-06 00:00:00
```

## Hintergrundwissen:
Das Parsen von Datumsstrings ist in der Programmierung tief verwurzelt. In der Vergangenheit behelften sich Entwickler oft mit `strtotime()` und den `date()` Funktionen. Doch diese Verfahren hatten ihre Tücken bei komplexen oder nicht standardisierten Formaten.

Die `DateTime` Klasse, eingeführt in PHP 5.2.0, bietet eine objektorientierte Lösung, mit der man viel präziser arbeiten kann. Sie versteht eine Vielzahl von Formaten und Zeitangaben und kann Zeitzonen verarbeiten. Für die meisten modernen Anwendungen ist die `DateTime` Klasse deshalb der Standard.

Alternativen zum PHP-Core sind Bibliotheken wie Carbon, ein Erweiterungspaket, das auf `DateTime` aufbaut und zusätzliche Funktionalitäten anbietet.

Wichtig ist auch, dass beim Parsen von Datumsstrings in verschiedenen Kulturen/Regionen unterschiedliche Formatierungen zu beachten sind – etwas, das `DateTime` mit den Lokalisierungs-Optionen steuern kann.

## Siehe auch:
- Die offizielle PHP-Dokumentation über die `DateTime` Klasse: https://www.php.net/manual/de/class.datetime.php
- Informationen zu `strtotime()`: https://www.php.net/manual/de/function.strtotime.php
- Carbon, eine Bibliothek für Datums- und Zeiteinstellungen in PHP: https://carbon.nesbot.com
