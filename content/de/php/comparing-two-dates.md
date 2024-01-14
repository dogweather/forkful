---
title:                "PHP: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Datumsangaben ist ein häufiges Szenario in der Programmierung. Oftmals müssen wir kontrollieren, ob ein Datum vor oder nach einem bestimmten Datum liegt, um entsprechende Aktionen durchzuführen. In diesem Blogbeitrag werden wir uns ansehen, wie wir in PHP zwei Datumswerte vergleichen können.

## Wie geht man vor

Um zwei Datumsangaben in PHP zu vergleichen, können wir die Funktion `strtotime()` verwenden. Diese konvertiert einen String in einen Unix-Timestamp, welcher eine ganze Zahl repräsentiert, die die Sekunden seit dem 1. Januar 1970 um 00:00:00 UTC angibt. Dadurch können wir leichter mit den Datumsangaben arbeiten und sie vergleichen.

Lassen Sie uns ein Beispiel betrachten:

```PHP
$erstesDatum = "2021-01-01";
$zweitesDatum = "2020-12-31";

$erstesDatumTimestamp = strtotime($erstesDatum);
$zweitesDatumTimestamp = strtotime($zweitesDatum);

if ($erstesDatumTimestamp > $zweitesDatumTimestamp) {
  echo "Das erste Datum liegt nach dem zweiten Datum";
} else if ($erstesDatumTimestamp < $zweitesDatumTimestamp) {
  echo "Das erste Datum liegt vor dem zweiten Datum";
} else {
  echo "Die beiden Daten sind gleich";
}
```

Ausgabe:

```
Das erste Datum liegt nach dem zweiten Datum
```

Wie Sie sehen können, haben wir die String-Datumsangaben in Unix-Timestamps konvertiert und anschließend miteinander verglichen. Somit können wir nun feststellen, welches Datum nach dem anderen liegt.

## Tiefergehende Informationen

Es gibt noch weitere Möglichkeiten, wie wir Datumswerte in PHP vergleichen können. So könnten wir beispielsweise auch die Funktionen `date_diff()` oder `DateTime::diff()` verwenden, um die Differenz zwischen zwei Datumsangaben zu berechnen. Auch die Verwendung von Vergleichsoperatoren wie `<`, `>` oder `==` ist möglich, wenn die Datumsangaben in einem für die Vergleichsoperatoren geeigneten Format vorliegen.

Es ist wichtig zu beachten, dass bei der Verwendung von verschiedenen Datumsformaten und Zeitzonen einige Einschränkungen und Probleme auftreten können. Daher ist es ratsam, immer die PHP-Dokumentation zu konsultieren, um sicherzustellen, dass Ihr Code korrekt funktioniert.

## Siehe auch

- Offizielle PHP-Dokumentation zur `strtotime()`-Funktion: https://www.php.net/manual/de/function.strtotime.php
- Dokumentation zur `date_diff()`-Funktion: https://www.php.net/manual/de/function.date-diff.php
- Informationen zur `DateTime::diff()`-Methode: https://www.php.net/manual/de/datetime.diff.php