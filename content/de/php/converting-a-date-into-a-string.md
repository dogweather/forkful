---
title:                "PHP: Ein Datum in eine Zeichenkette konvertieren"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung kann es häufig notwendig sein, ein Datum in eine Zeichenkette umzuwandeln, um es zu speichern oder anzuzeigen. Hier erfahren Sie, wie Sie dieses nützliche Feature in PHP nutzen können.

## Wie geht man vor

Das Konvertieren eines Datums in eine Zeichenkette kann mit der Funktion `date()` in PHP durchgeführt werden. Diese nimmt zwei Parameter an: das Dateiformat und das Datum, das in eine Zeichenkette umgewandelt werden soll. Hier ist ein Beispielcode:

```PHP
$datum = time();
$zeichenkette = date("d.m.Y", $datum);
echo $zeichenkette;
```

Die Variable $datum kann entweder ein tatsächliches Datum in Form eines Zeitstempels oder das aktuelle Datum sein, das durch die Funktion `time()` erzeugt wird. Die Formatierungsoptionen für `date()`, wie im obigen Beispiel verwendet, entsprechen der Syntax von `strftime()`, einer Funktion, die das Formatieren von Zeichenketten basierend auf Datum und Uhrzeit ermöglicht.

Im obigen Beispiel würde die Nummernkombination `d.m.Y` für das Datum "01.01.2021" die Zeichenkette "01.01.2021" erzeugen.

## Tiefergehender Einblick

Die Funktion `date()` bietet viele weitere Formatierungsoptionen für die Konvertierung von Datum in eine Zeichenkette. Eine vollständige Liste dieser Optionen kann in der offiziellen PHP-Dokumentation gefunden werden.

Einige der häufig verwendeten Optionen sind:

- `d` für den Tag als zweistellige Zahl (01-31)
- `m` für den Monat als zweistellige Zahl (01-12)
- `Y` für das Jahr als vierstellige Zahl (z.B. 2021)
- `l` für den Namen des Wochentags (z.B. Montag)
- `F` für den vollen Namen des Monats (z.B. Januar)

Es ist auch möglich, eigene Formate zu erstellen, indem man Zeichen wie Bindestriche oder Schrägstriche zwischen den verschiedenen Formatoptionen in die Zeichenkette einbaut.

## Siehe auch

- [Offizielle PHP-Dokumentation zu `date()`](https://www.php.net/manual/de/function.date.php)
- [Weitere Informationen zu `strftime()` und seinen Formatierungsoptionen](https://www.php.net/manual/de/function.strftime.php)
- [Ein Praxisbeispiel für die Verwendung von `date()`](https://www.geeksforgeeks.org/how-to-convert-date-object-to-date-string-in-php/)