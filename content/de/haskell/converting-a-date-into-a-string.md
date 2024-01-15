---
title:                "Ein Datum in eine Zeichenfolge umwandeln"
html_title:           "Haskell: Ein Datum in eine Zeichenfolge umwandeln"
simple_title:         "Ein Datum in eine Zeichenfolge umwandeln"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum 
Warum sollte man ein Datum in einen String umwandeln wollen? Nun, es gibt viele mögliche Gründe. Vielleicht müssen Sie ein Datumstempel für eine Datenbank oder eine Textausgabe vorbereiten, oder Sie wollen einfach nur ein bestimmtes Datumsformat für Ihre Anwendung verwenden. In jedem Fall ist die Fähigkeit, ein Datum in einen String umzuwandeln, ein wichtiger Bestandteil der Programmierung.

## Wie geht das 
Ein einfacher Weg, ein Datum in einen String umzuwandeln, ist die Verwendung der `formatTime` Funktion aus dem `Data.Time` Modul. Hier ist ein Beispiel, das das aktuelle Datum in das gängige Format "TT / MM / JJJJ" umwandelt:

```Haskell
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

main = do
    now <- getCurrentTime
    let dateStr = formatTime defaultTimeLocale "%d/%m/%Y" now
    print dateStr
```

Dieses Programm importiert die benötigten Module, ruft `getCurrentTime` auf, um das aktuelle Datum zu erhalten, und verwendet dann `formatTime` mit dem gewünschten Format und dem `defaultTimeLocale` Parameter, um es in einen String umzuwandeln. Die Ausgabe ist in diesem Fall "03/08/2021".

## Tiefer eintauchen 
Nun, da Sie wissen, wie man ein Datum in einen String umwandeln kann, lassen Sie uns etwas tiefer eintauchen. Die `formatTime` Funktion akzeptiert auch benutzerdefinierte Formate, die Sie durch das Erstellen Ihrer eigenen `TimeLocale`-Objekte definieren können. Dies kann hilfreich sein, wenn Sie ein spezielles Datumformat benötigen oder Ihre Anwendung in einer anderen Sprache ausgeben möchten.

Außerdem gibt es noch die `showGregorian` Funktion aus dem `Data.Time.Calendar` Modul, die ein Datum im gregorianischen Kalender in einem bestimmten Format als String darstellt. Dies kann nützlich sein, wenn Sie mit speziellen Datumsobjekten arbeiten, die nicht von `Data.Time` unterstützt werden.

## Siehe auch 
- [Haskell Wiki - Adding time and dates](https://wiki.haskell.org/Adding_time_and_dates)
- [Haskell - Data Time library](https://hackage.haskell.org/package/time/docs/Data-Time.html)