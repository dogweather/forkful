---
date: 2024-01-20 17:33:42.927532-07:00
description: "How to: (Wie geht das?) Angenommen, wir haben zwei Daten und wollen\
  \ wissen, ob eines vor oder nach dem anderen liegt. Hier ein schn\xF6rkelloser PHP-\u2026"
lastmod: '2024-04-05T21:53:55.869354-06:00'
model: gpt-4-1106-preview
summary: (Wie geht das?) Angenommen, wir haben zwei Daten und wollen wissen, ob eines
  vor oder nach dem anderen liegt.
title: Vergleich von zwei Daten
weight: 27
---

## How to:
(Wie geht das?)
Angenommen, wir haben zwei Daten und wollen wissen, ob eines vor oder nach dem anderen liegt. Hier ein schnörkelloser PHP-Schnipsel:

```php
<?php
$date1 = new DateTime("2023-01-01");
$date2 = new DateTime("2023-12-31");

if ($date1 < $date2) {
    echo "Datum1 liegt vor Datum2.";
} else {
    echo "Datum1 liegt nicht vor Datum2.";
}
?>
```

Ergebnis:
```
Datum1 liegt vor Datum2.
```

Wenn wir wissen möchten, wie viele Tage der Unterschied ist, dann sieht das so aus:

```php
<?php
$diff = $date1->diff($date2);
echo $diff->days . " Tage Unterschied.";
?>
```

Ergebnis:
```
364 Tage Unterschied.
```

## Deep Dive:
(Tiefer tauchen)
Früher war der Datumvergleich in PHP ein Krampf. Man jonglierte mit `strtotime()` und anderen Funktionen. Seit PHP 5.2 haben wir das `DateTime`-Objekt, das vieles vereinfacht. 

Alternativen? Klar, die gibt's. Man kann die Unix Timestamps vergleichen, `strtotime()` nutzen oder externe Bibliotheken wie Carbon für PHP einsetzen, wenn es noch komfortabler sein soll.

Implementierungsdetails? Beim Vergleich mit `<`, `>` und `==` wird unter der Haube die Unix Timestamp genutzt. Also Vorsicht mit Zeitzonen! Die `diff`-Methode von `DateTime` gibt ein `DateInterval` zurück, das reichlich Infos wie die Anzahl der Tage bietet.

## See Also:
(Siehe auch)
- [Die offizielle PHP-Dokumentation für DateTime](https://www.php.net/manual/en/class.datetime.php)
- [Carbon: Eine einfache API-Erweiterung für DateTime](https://carbon.nesbot.com/)
- [PHP.net's DateInterval Dokumentation](https://www.php.net/manual/en/class.dateinterval.php)
