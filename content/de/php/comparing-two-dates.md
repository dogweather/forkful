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

## Warum

Es gibt viele Gründe, warum man in PHP zwei verschiedene Daten vergleichen möchte. Zum Beispiel um festzustellen, ob eine bestimmte Aktion bereits in der Vergangenheit passiert ist oder ob ein Datum in einem bestimmten Zeitrahmen liegt.

## Wie das geht

Um zwei Datumswerte miteinander zu vergleichen, können die Funktionen `strtotime()` und `date()` verwendet werden. Diese Funktionen konvertieren Datumswerte in einen Unix-Timestamp, der dann einfach verglichen werden kann.

Beispiel:

```PHP
$date_one = strtotime("14 February 2020");
$date_two = strtotime("20 March 2020");

if ($date_one < $date_two) {
    echo date("d/m/Y", $date_one) . " liegt vor " . date("d/m/Y", $date_two);
} else {
    echo date("d/m/Y", $date_two) . " liegt vor " . date("d/m/Y", $date_one);
}
```

Output:

```text
14/02/2020 liegt vor 20/03/2020
```

## Tiefere Einblicke

Es ist wichtig zu beachten, dass bei der Verwendung von `strtotime()` und `date()` die Datumsformate beachtet werden müssen. PHP kann verschiedene Datumsformate verstehen, aber es ist immer ratsam, das Datum im ISO-8601-Format anzugeben (z.B. "YYYY-MM-DD"). Weitere Informationen zu Datumsformaten in PHP findest du [hier](https://www.php.net/manual/de/datetime.formats.php).

Außerdem ist es möglich, auch die genaue Uhrzeit in den Vergleich einzubeziehen, indem man den Zeitstempel mit Stunden, Minuten und Sekunden ergänzt (z.B. `strtotime("14 February 2020 12:30:00")`).

Eine weitere Möglichkeit, um zwei Datumswerte in PHP zu vergleichen, ist die Verwendung der `DateTime` Klasse. Diese bietet noch mehr Funktionalitäten und ist besonders nützlich, wenn mit wiederkehrenden Ereignissen oder Zeitzonen gearbeitet wird. Weitere Informationen zu diesem Ansatz findest du [hier](https://www.php.net/manual/de/class.datetime.php).

## Siehe auch

- [PHP Dateien vergleichen](https://www.php.net/manual/de/function.filemtime.php)
- [PHP Daten und Zeiten vergleichen](https://www.php.net/manual/de/datetime.diff.php)
- [ISO-8601 Datumsspezifikation](https://www.iso.org/iso-8601-date-and-time-format.html)