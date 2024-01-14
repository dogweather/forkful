---
title:    "PHP: Zwei Daten vergleichen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum

Vergleichen von Datumsangaben ist ein sehr nützliches Werkzeug in der Programmierung. Es erlaubt uns, festzustellen, ob ein bestimmtes Datum vor oder nach einem anderen Datum liegt, was uns bei der Sortierung und Verarbeitung von Daten helfen kann.

## Wie

```PHP
<?php 
// Beispieldatumsangaben
$date1 = "2021-04-15";
$date2 = "2021-04-20";

// Vergleichen der Datumsangaben
if($date1 < $date2) {
    echo "Das erste Datum liegt vor dem zweiten.";
} elseif($date1 > $date2) {
    echo "Das erste Datum liegt nach dem zweiten.";
} else {
    echo "Die Datumsangaben sind gleich.";
}
```

Output:
```
Das erste Datum liegt vor dem zweiten.
```

## Tiefergehende Analyse

Beim Vergleichen von Datumsangaben ist es wichtig zu wissen, dass sie in PHP als Strings behandelt werden. Das bedeutet, dass das Vergleichen von Datumsangaben mit dem kleiner/größer-Operator funktioniert, da diese Operatoren Strings nach ihrer lexikographischen Ordnung vergleichen.

Es ist jedoch wichtig zu beachten, dass das Datum im Format "Jahr-Monat-Tag" sein muss, um korrekt verglichen zu werden. Andernfalls kann es zu unerwarteten Ergebnissen führen.

## Siehe auch

- [PHP: Datums- und Zeitfunktionen](https://www.php.net/manual/de/ref.datetime.php)
- [JavaScript: Vergleich von Datumsangaben](https://www.tutdepot.com/code/javascript/how-to-compare-dates/)
- [Python: Vergleich von Datumsangaben](https://www.programiz.com/python-programming/datetime/compare-datetime)