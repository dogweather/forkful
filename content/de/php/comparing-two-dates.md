---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Vergleichen zweier Daten in PHP beinhaltet, herauszufinden, welches Datum jünger oder älter ist. Programmierer machen dies, um Ereignisse nach Zeit zu sortieren oder um Zeitspannen zu berechnen.

## So funktioniert's:

Es gibt verschiedene Methoden, zwei Daten in PHP zu vergleichen. Eine einfache Methode nutzt die eingebaute Funktion `strtotime()`:

```PHP
$datum1 = "2022-06-19";
$datum2 = "2022-06-21";

if (strtotime($datum1) > strtotime($datum2)) {
    echo "$datum1 ist größer als $datum2";
} else {
    echo "$datum1 ist kleiner als $datum2";
}
```
Ausgabe:
```
2022-06-19 ist kleiner als 2022-06-21
```

Eine alternative Methode benutzt die `DateTime()` Klasse:

```PHP
$DatumObj1 = new DateTime('2022-06-19');
$DatumObj2 = new DateTime('2022-06-21');

if ($DatumObj1 > $DatumObj2) {
    echo "$DatumObj1->date ist größer als $DatumObj2->date";
} else {
    echo "$DatumObj1->date ist kleiner als $DatumObj2->date";
}
```
Ausgabe:
```
2022-06-19 ist kleiner als 2022-06-21
```

## Vertiefung:

Das Datum und die Zeit zu vergleichen ist eine häufige Aufgabe in der Programmierung, vor allem bei der Verarbeitung von Logs oder'scheduling' Events. Historisch gesehen haben Entwickler dafür manchmal rudimentäre Methoden genutzt und die Zeichenketten direkt verglichen. Dies kann jedoch ungenau sein, insbesondere bei unterschiedlichen Formaten und Zeitzonen. Modernes PHP bietet leistungsstarke Funktionen wie `strtotime()` und die `DateTime` Klasse, um genaue Vergleiche zu ermöglichen.

Alternativen zum Vergleichen von Daten sind z.B. Pear's Date Package oder Drittanbieter-Bibliotheken wie Carbon. Diese bieten erweiterte Funktionen, können aber auch unnötigen Ballast für einfache Anforderungen darstellen.

Wichtig bei der Implementierung ist, dass sowohl `strtotime()` als auch `DateTime` die Zeitzonen berücksichtigen. Daher ist es wichtig, immer die korrekte Zeitzone zu setzen oder UTC zu verwenden.

## Siehe Auch:

PHP Dokumentation zu strtotime(): https://www.php.net/manual/de/function.strtotime.php

PHP Dokumentation zu DateTime: https://www.php.net/manual/de/class.datetime.php

Pear Date Package: https://pear.php.net/package/Date

Carbon Bibliothek: https://carbon.nesbot.com/