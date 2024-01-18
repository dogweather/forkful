---
title:                "Datum aus einem String extrahieren"
html_title:           "PHP: Datum aus einem String extrahieren"
simple_title:         "Datum aus einem String extrahieren"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bezieht sich auf die Konvertierung eines Textes, der ein Datum enthält, in ein verwendbares Datumsformat. Programmierer tun dies, um mit Datumswerten zu arbeiten, die in verschiedenen Formaten vorliegen können und um die Lesbarkeit und Genauigkeit von Datumsangaben zu gewährleisten.

## Wie geht's?
Hier sind zwei Beispiele, wie man in PHP ein Datum aus einem String parsen kann:

```PHP
<?php
$datum1 = date_create_from_format("d/m/Y", "20/01/2020");
echo date_format($datum1, "Y-m-d");
?>

```
Ergebnis: 2020-01-20

```PHP
<?php
$datum2 = strtotime("12 March 2021");
echo date("Y-m-d", $datum2);
?>
```
Ergebnis: 2021-03-12

In beiden Fällen wird der Text in ein gültiges Datum umgewandelt, das dann in einem bestimmten Format ausgegeben werden kann. Die Funktion `date_create_from_format` ermöglicht dabei die Verwendung eines benutzerdefinierten Datumsformats, während die Funktion `strtotime` verschiedene Textformate des Datums unterstützt.

## Tiefere Einblicke
Das Parsen von Daten aus einem String ist seit jeher eine wichtige Funktion in der Programmierung, da Datumwerte in verschiedenen Formaten vorliegen können und es wichtig ist, diese korrekt zu interpretieren. Neben den gezeigten Beispielen gibt es auch andere Methoden, um dieses Problem anzugehen, wie zum Beispiel die Verwendung von regulären Ausdrücken oder das Parsen von Datumsangaben aus verschiedenen Sprachen.

## Siehe auch
- [PHP date() Funktion](https://www.php.net/manual/de/function.date.php)
- [PHP date_create_from_format() Funktion](https://www.php.net/manual/de/function.date-create-from-format.php)
- [PHP strtotime() Funktion](https://www.php.net/manual/de/function.strtotime.php)