---
title:    "Haskell: Vergleich von zwei Daten"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Vergleichen von Daten ist eine häufige Aufgabe in der Programmierung, insbesondere in Haskell. Es kann verwendet werden, um zu überprüfen, ob eine bestimmte Zeit bereits vergangen ist, an welchem Punkt im Jahr ein Ereignis stattfindet oder um einen bestimmten Zeitraum zu berechnen. Das Vergleichen von Daten kann auch hilfreich sein, um Zeitstempel in einer Datenbank zu sortieren oder um zu überprüfen, ob eine angegebene Frist bereits abgelaufen ist.

## Wie man Daten vergleicht

Das Vergleichen von Daten in Haskell ist relativ einfach, solange man die richtigen Funktionen verwendet. Zunächst muss man die beiden Daten in das Format "Jahr-Monat-Tag" übersetzen, da Haskell diese Form benötigt, um Daten zu vergleichen. Wir können dies mit der Funktion `fromGregorian` aus dem Modul `Data.Time.Calendar` tun.

```Haskell
import Data.Time.Calendar

-- Two dates to compare
date1 = fromGregorian 2021 10 17
date2 = fromGregorian 2022 4 1

-- Compare dates
compareDates :: Day -> Day -> Ordering
compareDates date1 date2 = compare date1 date2

-- Print output (GT for Greater Than, LT for Less Than, EQ for Equal)
print (compareDates date1 date2)
```

Dieses Beispiel gibt "GT" aus, da `date2` ein späteres Datum ist als `date1`. Es ist auch möglich, zwei Daten direkt zu vergleichen, anstatt die `fromGregorian` Funktion zu verwenden.

```Haskell
-- Compare dates
compareDates :: Day -> Day -> Ordering
compareDates date1 date2 = compare (fromGregorian 2021 10 17) (fromGregorian 2022 4 1)

-- Print output (GT for Greater Than, LT for Less Than, EQ for Equal)
print (compareDates)
```

Dieses Beispiel gibt ebenfalls "GT" aus. Es ist wichtig zu beachten, dass das Vergleichen von Daten immer "Ordering" als Rückgabetyp hat und nicht einfach "Bool". Dies liegt daran, dass es mehrere Möglichkeiten gibt, wie Daten miteinander verglichen werden können (z.B. "GT" für größer als, "EQ" für gleich usw.).

## Tiefere Einblicke

Es gibt viele weitere Funktionen, die bei der Arbeit mit Daten in Haskell hilfreich sein können. Zum Beispiel gibt es Funktionen wie `diffDays`, `addDays` und `isLeapYear`, die für bestimmte Aufgaben nützlich sein können. Die Verwendung dieser Funktionen erfordert jedoch ein Verständnis der zugrunde liegenden Datentypen und Module. Es ist daher empfehlenswert, sich eingehender mit der Dokumentation für das Modul `Data.Time.Calendar` auseinanderzusetzen.

## Siehe auch

- [Dokumentation für das Modul `Data.Time.Calendar`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html)
- [Video-Tutorial zum Vergleichen von Daten in Haskell](https://www.youtube.com/watch?v=Rj_gF7rEryo)
- [Programmierung mit Haskell: Eine Einführung](https://www.buch.de/shop/home/artikeldetails/ID63441034.html) (Buchempfehlung)