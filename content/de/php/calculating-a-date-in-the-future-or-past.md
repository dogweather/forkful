---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "PHP: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich damit beschäftigen, ein Datum in der Zukunft oder Vergangenheit zu berechnen? Ganz einfach: Es kann uns helfen, bestimmte Aufgaben in unserer Programmierung zu erleichtern. Zum Beispiel könnten wir damit automatisch Geburtstagsnachrichten versenden oder Rechnungen mit fälligen Zahlungsfristen generieren.

## Wie man es macht
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir den eingebauten PHP Befehl `strtotime()` verwenden. Dieser Befehl ermöglicht es uns, einen Zeitstempel aus einer Zeichenkette zu generieren. Hier ist ein Beispiel, um das Datum in sieben Tagen zu berechnen:

```PHP
<?php
$zukunftsDatum = strtotime("+7 days");
echo date("Y-m-d", $zukunftsDatum);
```

Dieser Code erzeugt ein neues Datum, das sieben Tage in der Zukunft liegt, und gibt es im Format `Jahr-Monat-Tag` aus. Wir können auch negative Werte verwenden, um ein Datum in der Vergangenheit zu berechnen. Hier ist ein Beispiel, um das Datum vor zwei Monaten zu berechnen:

```PHP
<?php
$vergangenheitsDatum = strtotime("-2 months");
echo date("d.m.Y", $vergangenheitsDatum);
```

In diesem Fall verwenden wir ein anderes Datumsformat und erhalten das Datum im Format `Tag.Monat.Jahr`.

## Tiefer gehende Informationen
Der `strtotime()` Befehl unterstützt auch die Verwendung von relativen Angaben wie "next Monday" (nächster Montag) oder "last Friday" (letzter Freitag) anstelle von Zahlen. Außerdem können wir auch Datum und Uhrzeit kombinieren, indem wir eine Zeichenkette mit dem gewünschten Datum und einer Uhrzeit an den Befehl übergeben. Wir können auch einen optionalen zweiten Parameter übergeben, der als Basiszeit verwendet wird. Dadurch können wir zum Beispiel ein Datum in Bezug auf ein bestimmtes Datum berechnen, anstatt immer das aktuelle Datum zu verwenden.

## Siehe auch
- Dokumentation zu `strtotime()`: https://www.php.net/manual/de/function.strtotime.php
- Praktische Anwendungen für das Berechnen von Datumsangaben: https://www.php.net/manual/de/datetime.formats.relative.php