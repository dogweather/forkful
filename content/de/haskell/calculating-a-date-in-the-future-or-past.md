---
title:                "Haskell: Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Datumsangaben kann sehr nützlich sein, wenn man zum Beispiel eine Erinnerungsfunktion oder Terminplanung in seiner Haskell-Anwendung benötigt.

## Wie geht das?

```Haskell
import Data.Time.Calendar

-- Berechnung eines Datums in der Zukunft
let future = addDays 10 today
-- future wird das Datum in 10 Tagen enthalten

-- Berechnung eines Datums in der Vergangenheit
let past = addDays (-7) today
-- past wird das Datum vor 7 Tagen enthalten
```

Die `addDays` Funktion aus dem `Data.Time.Calendar` Modul ermöglicht es uns, ein bestimmtes Datumsobjekt (hier `today`) um eine bestimmte Anzahl an Tagen zu erweitern oder zu reduzieren.

## Tieferer Einblick

Für die Berechnung von Datumsangaben in der Zukunft oder Vergangenheit gibt es verschiedene Optionen. Neben der `addDays` Funktion gibt es auch noch `addMonths` und `addYears`, um Datumsangaben um Monate bzw. Jahre zu erweitern. Diese Funktionen funktionieren ähnlich wie `addDays`.

Außerdem gibt es auch Funktionen wie `diffDays`, `diffMonths` und `diffYears`, mit denen die Differenz zwischen zwei Datumsangaben berechnet werden kann.

## Siehe auch
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#v:addDays
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#v:addMonths
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#v:addYears
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#v:diffDays
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#v:diffMonths
- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#v:diffYears