---
title:                "Vergleich von zwei Daten"
html_title:           "Haskell: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man zwei Daten vergleichen möchte. Dies kann beispielsweise bei der Sortierung von Ereignissen oder der Überprüfung von Altersbeschränkungen hilfreich sein.

## Wie geht das?
Um zwei Daten in Haskell zu vergleichen, können wir die integrierte Funktion `compare` verwenden. Diese Funktion akzeptiert zwei Daten und gibt als Ergebnis eine Ordnungsrelation zurück. Hier ist ein Beispielcode, der zwei Daten vergleicht:
```Haskell
compareDates :: Day -> Day -> Ordering
compareDates date1 date2 = compare date1 date2
```
Das Ergebnis dieser Funktion kann dann für weitere Vergleiche wie `==, /=, >, <` verwendet werden. Hier ist ein Beispiel, das das Ergebnis der `compareDates`-Funktion verwendet, um zu überprüfen, ob ein Datum vor einem anderen liegt:
```Haskell
isBefore :: Day -> Day -> Bool
isBefore date1 date2 = case compareDates date1 date2 of
                        LT -> True
                        _ -> False
```
Die `compare`-Funktion kann auch auf andere Datentypen angewendet werden, solange sie eine Instanz der `Ord`-Klasse haben.

## Tieferer Einblick
Die `compare`-Funktion verwendet eine Ordnungsrelation, um die Reihenfolge von Daten zu bestimmen. Diese Relation kann entweder durch eine aufsteigende oder absteigende Reihenfolge definiert werden. In der Standardimplementierung von Haskell ist aufsteigende Reihenfolge die Standardkonvention für Daten. Dies bedeutet, dass das frühere Datum als kleinerer Wert behandelt wird und somit eine Ordnung von älter zu neuer erstellt wird. Um absteigende Reihenfolge zu verwenden, kann man die `reverse`-Funktion verwenden, um die Reihenfolge umzukehren.

## Siehe auch
- [Haskell-Dokumentation über die `compare`-Funktion](https://www.haskell.org/onlinereport/standard-prelude.html#t:Ord)
- [Eine Einführung in Haskell](https://dev.to/thomashoneyman/an-introduction-to-haskell-2jkj)