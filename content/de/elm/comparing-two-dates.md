---
title:                "Elm: Vergleich von zwei Daten"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
Wenn Sie gerade erst anfangen, mit Elm zu programmieren, fragen Sie sich vielleicht, warum Sie sich die Zeit nehmen sollten, zwei Daten zu vergleichen. Der Vergleich von Daten kann sehr hilfreich sein, um Zeitstempel zu verarbeiten oder um zu überprüfen, ob ein Datum in einem bestimmten Zeitraum liegt. Lesen Sie weiter, um zu erfahren, wie Sie dies mit Elm ganz einfach machen können.

## Wie geht's
Um zwei Daten in Elm zu vergleichen, können Sie die Funktion `compare` aus dem Kernmodul `Basics` verwenden. Diese Funktion nimmt zwei Werte und gibt einen von drei möglichen Werten zurück: `LT`, `EQ` oder `GT`, je nachdem, ob der erste Wert kleiner, gleich oder größer als der zweite Wert ist.

```Elm
compare "2020-05-01" "2020-04-01" == GT

compare "2020-01-01 00:00:00" "2020-01-01 12:00:00" == LT
```

In diesen Beispielen vergleichen wir zwei Strings, die Datumsangaben darstellen. Beachten Sie, dass es wichtig ist, dass die Daten im gleichen Format dargestellt werden, damit der Vergleich korrekt ausgeführt werden kann.

## Tiefentauchen
Die Funktion `compare` mag auf den ersten Blick einfach erscheinen, aber es lohnt sich, tiefer in die Details einzutauchen. Zum Beispiel können wir `compare` nicht nur für den Vergleich von Daten nutzen, sondern auch für die Vergleich von anderen Werten wie Zahlen und Zeichenketten.

``` Elm
compare 5 10 == LT

compare "apple" "banana" == LT
```

Eine weitere wichtige Sache zu beachten ist, dass der Vergleich auch hierarchisch funktioniert. Wenn wir zum Beispiel zwei Daten vergleichen, die unterschiedliche Granularitäten haben, wird die höhere Granularität (z.B. Monat) als wichtiger betrachtet als die niedrigere Granularität (z.B. Tag).

``` Elm
compare "2020-01-01" "2020-01-01 12:00:00" == EQ
```

Schließlich ist es auch wichtig zu beachten, dass der Vergleich von Daten standardmäßig vom ältesten bis zum neuesten Datum funktioniert. Wenn Sie dies ändern möchten, können Sie einen Custom Comparator verwenden.

## Siehe auch
- Elm Offizielle Dokumentation zu `compare`: https://package.elm-lang.org/packages/elm/core/latest/Basics#compare
- Elm Comparators Paket: https://package.elm-lang.org/packages/elm/core/latest/Basics#comparing