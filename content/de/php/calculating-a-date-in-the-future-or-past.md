---
title:                "PHP: Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Die Berechnung eines Datums in der Zukunft oder Vergangenheit ist ein wichtiger Teil der Programmierung, der in vielen Bereichen Anwendung finden kann. Ob es darum geht, Rechnungen zu erstellen, den Ablauf von Projekten zu planen oder Termine zu verwalten, die Fähigkeit, ein Datum in PHP zu berechnen, kann sehr nützlich sein.

## Wie geht das

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, benötigen Sie das aktuelle Datum als Ausgangspunkt. Dies kann mithilfe der Funktion `date()` in PHP geschehen. Diese gibt das aktuelle Datum im gewünschten Format zurück.

Als nächstes müssen Sie entscheiden, wie viele Tage Sie dem aktuellen Datum hinzufügen oder davon abziehen möchten. Dies können Sie mithilfe der Funktion `strtotime()` tun. Diese Funktion nimmt einen String als Argument und konvertiert ihn in eine Datums- und Zeitangabe. Zum Beispiel würde `strtotime("+1 week")` dem aktuellen Datum eine Woche hinzufügen.

Schließlich können Sie das berechnete Datum mithilfe der Funktion `date()` erneut formatieren, um es in einem bestimmten Format auszugeben. Hier ist ein Beispielcode, der dem aktuellen Datum drei Tage hinzufügt und das Ergebnis im Format "d.m.Y" (Tag.Monat.Jahr) ausgibt:

```PHP
$heutiges_datum = date("d.m.Y");
$zukuenftiges_datum = date("d.m.Y", strtotime("+3 days"));
echo "Das Datum in drei Tagen ist: $zukuenftiges_datum";
```

Dieser Code würde die folgende Ausgabe erzeugen:

```
Das Datum in drei Tagen ist: 16.06.2021
```

## Tiefer eintauchen

Es gibt viele weitere Möglichkeiten, ein Datum in der Zukunft oder Vergangenheit zu berechnen. Zum Beispiel können Sie auch Monate, Jahre oder sogar spezifische Wochentage hinzufügen oder abziehen. Sie können auch bestimmte Datumsformate verwenden, um das Ergebnis anzupassen.

Eine andere nützliche Funktion ist `mktime()`, mit der Sie ein Datum aus bestimmten Zeitangaben erstellen können. Dies könnte hilfreich sein, wenn Sie ein bestimmtes Datum als Ausgangspunkt haben und in die Zukunft oder Vergangenheit rechnen möchten.

Es ist auch wichtig zu beachten, dass PHP verschiedene Funktionen anbietet, um mit Datums- und Zeitangaben in verschiedenen Zeitzonen zu arbeiten. Diese können sehr hilfreich sein, wenn Sie mit internationalen Daten arbeiten.

## Siehe auch

- [PHP-Dokumentation zu `date()`](https://www.php.net/manual/de/function.date.php)
- [PHP-Dokumentation zu `strtotime()`](https://www.php.net/manual/de/function.strtotime.php)
- [PHP-Dokumentation zu `mktime()`](https://www.php.net/manual/de/function.mktime.php)