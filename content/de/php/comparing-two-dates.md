---
title:                "PHP: Den Vergleich von zwei Datumsangaben"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Manchmal müssen wir in der Programmierung zwei verschiedene Daten miteinander vergleichen. Das kann zum Beispiel nützlich sein, um festzustellen, ob ein Ereignis in der Vergangenheit oder Zukunft liegt oder um eine sortierte Liste zu erstellen. In dieser Blog-Post werden wir uns ansehen, wie man mit PHP zwei Daten vergleichen kann.

## Wie geht man vor?

Man kann mit der `DateTime` Klasse in PHP Daten und Zeiten darstellen und vergleichen. Hier ist ein Beispiel, wie man zwei Daten miteinander vergleichen kann:

```PHP
$datum1 = new DateTime('2020-01-01');
$datum2 = new DateTime('2020-01-15');

if($datum1 < $datum2) {
  echo 'Das erste Datum liegt vor dem zweiten Datum';
} elseif($datum1 > $datum2) {
  echo 'Das erste Datum liegt nach dem zweiten Datum';
} else {
  echo 'Beide Daten sind gleich';
}
```

Die Ausgabe würde dabei lauten: `Das erste Datum liegt vor dem zweiten Datum`.

Man kann auch die `diff()` Funktion verwenden, um die Differenz zwischen zwei Daten in Tagen, Monaten oder Jahren zu erhalten. Hier ist ein Beispiel:

```PHP
$datum1 = new DateTime('2020-01-01');
$datum2 = new DateTime('2020-01-15');

$differenz = $datum2->diff($datum1);

echo $differenz->format('%R%a Tage'); // gibt "+14 Tage" aus
```

Weitere Beispiele und Informationen findest du in der offiziellen PHP Dokumentation zur [DateTime Klasse](https://www.php.net/manual/de/class.datetime.php).

## Tiefere Einblicke

Wenn man zwei Daten vergleicht, ist es wichtig zu beachten, dass es auch auf die Zeit ankommt. Das heißt, dass zwei Daten, die das gleiche Datum haben, aber verschiedene Zeiten, möglicherweise auch als ungleich betrachtet werden. Deshalb ist es sinnvoll, bei Vergleichen die gleiche Uhrzeit zu berücksichtigen oder die Zeitkomponente zu ignorieren.

Eine andere Sache, die man beachten muss, ist die Zeitzonen. Wenn man zum Beispiel Daten aus verschiedenen Zeitzonen vergleicht, kann es zu unerwarteten Ergebnissen kommen. Es ist daher wichtig, sicherzustellen, dass alle Daten in der gleichen Zeitzone sind, bevor man sie vergleicht.

## Siehe auch

Für weitere Informationen und Beispiele zum Vergleichen von Daten mit PHP empfehle ich folgende Links:

- [Offizielle PHP Dokumentation](https://www.php.net/manual/de/class.datetime.php)
- [Tutorial: Arbeiten mit Datum und Uhrzeit in PHP](https://www.php-einfach.de/php-tutorial/zeit-und-datum/)
- [Video-Tutorial: Wie man mit Datum und Zeit in PHP arbeitet](https://www.youtube.com/watch?v=VEd1kpQDJ5c)