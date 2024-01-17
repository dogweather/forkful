---
title:                "Vergleich von zwei Daten"
html_title:           "PHP: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ist ein häufiges Problem, dem sich Programmierer gegenübersehen. Es bezieht sich darauf, wie zwei Datumsangaben miteinander verglichen werden können, um festzustellen, welches früher oder später ist. Programmierer müssen dies tun, um verschiedene Aufgaben zu erfüllen, wie z.B. das Sortieren von Datumsangaben oder das Überprüfen von Fälligkeiten.

## Wie funktioniert's?
In PHP gibt es einige Methoden, um zwei Datumsangaben zu vergleichen. Die einfachste Methode ist die Verwendung der Vergleichsoperatoren "<" und ">". Hier ist ein Beispiel:

```PHP
$date1 = '2020-01-01';
$date2 = '2020-01-05';

if ($date1 < $date2) {
  echo $date1 . " ist früher als " . $date2;
} elseif ($date1 > $date2) {
  echo $date1 . " ist später als " . $date2;
} else {
  echo $date1 . " ist gleich " . $date2;
}

// Output: 2020-01-05 ist später als 2020-01-01
```

Eine andere Möglichkeit ist die Verwendung der PHP-Funktion "strtotime()", die einen Zeitstempel für ein Datum zurückgibt. Dieser Zeitstempel kann dann verglichen werden. Hier ist ein Beispiel:

```PHP
$date1 = '2020-01-01';
$date2 = '2020-01-05';

if (strtotime($date1) < strtotime($date2)) {
  echo $date1 . " ist früher als " . $date2;
} elseif (strtotime($date1) > strtotime($date2)) {
  echo $date1 . " ist später als " . $date2;
} else {
  echo $date1 . " ist gleich " . $date2;
}

// Output: 2020-01-05 ist später als 2020-01-01
```

## Tiefere Einblicke
Die Notwendigkeit, zwei Datumsangaben miteinander zu vergleichen, ist auf die Wichtigkeit von Zeit in der Programmierung zurückzuführen. Das Vergleichen von zwei Daten kann auch komplexer sein, wenn verschiedene Zeitzonen berücksichtigt werden müssen. In solchen Fällen ist es möglicherweise sinnvoller, die PHP-Klasse "DateTime" zu verwenden, die eine genauere und präzisere Art des Datumsvergleichs ermöglicht.

Es gibt auch alternative Methoden, um zwei Datumsangaben zu vergleichen, wie zum Beispiel die Verwendung der PHP-Funktion "date_diff()", die die Differenz zwischen zwei Datumsangaben zurückgibt. Programmierer können diese Funktion verwenden, um zu überprüfen, ob ein bestimmtes Datum innerhalb eines bestimmten Zeitraums liegt oder nicht.

Die Implementierung des Vergleichs von zwei Datumsangaben kann je nach Programmier-Sprache variieren. Es ist wichtig für Programmierer, die Syntax und Besonderheiten der verwendeten Sprache zu verstehen, um ein genaues Ergebnis zu erzielen.

## Siehe auch
Für weitere Informationen über das Vergleichen von zwei Datumsangaben in PHP, siehe:

- [PHP Manual - Datum und Zeit](https://www.php.net/manual/de/datetime.formats.php)
- [PHP Class - DateTime](https://www.php.net/manual/de/class.datetime.php)
- [PHP Function - date_diff()](https://www.php.net/manual/de/function.date-diff.php)