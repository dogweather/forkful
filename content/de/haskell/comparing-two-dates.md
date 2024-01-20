---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Vergleichen von zwei Daten besteht darin, zu entscheiden, ob ein Datum vor, nach oder gleich einem anderen ist. Programmierer machen das, um zeitbasierte Logiken wie Terminplaner oder Historiker in ihren Anwendungen zu implementieren.

## So geht's:

Im Folgenden finden Sie ein Beispiel, wie Sie two Dates in Haskell mithilfe des `Data.Time.Calendar`-Pakets vergleichen können. 

```Haskell
import Data.Time.Calendar

compareDates :: Day -> Day -> Ordering
compareDates d1 d2 = compare d1 d2
```

Nutzen Sie das obere Beispiel wie folgt:

```Haskell
main = do
   let date1 = fromGregorian 2020 02 08
   let date2 = fromGregorian 2020 12 31
   print(compareDates date1 date2)  
```

Es zeigt `LT` auf der Konsole, da das erste Datum, `date1`, weniger (früher) ist als das zweite Datum, `date2`.

## Tiefgehende Informationen

1. Historischer Kontext: Der `Data.Time.Calendar` ist ein essentieller Teil der `time` Bibliothek in Haskell, die ursprünglich im Jahr 2006 eingeführt wurde. Diese Bibliothek bietet umfangreiche Funktionen zur Datums- und Zeitmanipulation.

2. Alternativen: Falls Sie mehr Funktionalitäten benötigen, wie die Arbeit mit Zeitzonen, können Sie das `Data.Time`-Paket verwenden, das ergänzende Funktionen zur Verfügung stellt.

3. Implementierungsdetail: Die `compare`-Funktion in Haskell ist eine polyforme Funktion, die Teil der `Ord` Klasse ist. Bei der Anwendung auf zwei Argumente gibt sie eine von drei möglichen Antworten zurück: `LT` (Less Than, d.h. kleiner), `EQ` (Equal, d.h. gleich) oder `GT` (Greater Than, d.h. größer).

## Siehe auch

- Die offizielle Dokumentation für das `Data.Time.Calendar`-Paket bietet umfassende Informationen: [Hier klicken](https://hackage.haskell.org/package/time-1.10/docs/Data-Time-Calendar.html)

- Ein lehrreicher Blog-Artikel über Datum und Zeit in Haskell: [Hier klicken](https://two-wrongs.com/haskell-time-library-tutorial)

- Für tiefgründigere Diskussionen und Fragen können Sie die Haskell Community auf Stack Overflow besuchen: [Hier klicken](https://stackoverflow.com/questions/tagged/haskell)