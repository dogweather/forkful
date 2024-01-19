---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Umwandeln eines Datums in einen String in Haskell ist der Prozess, bei dem ein Datum zu einer menschenlesbaren Zeichenkette formatiert wird. Das ist nützlich, weil wir damit Daten besser darstellen und bearbeiten können.

## Anleitung:

Angenommen, Sie haben ein Datum im Format `UTCTime`. Sie können die `formatTime` Funktion aus dem `Data.Time.Format` Modul verwenden, um dieses Datum zu formatieren.

```Haskell
import Data.Time.Clock
import Data.Time.Format
import System.Locale

main = do
  currentTime <- getCurrentTime
  let StringDate = formatTime defaultTimeLocale "%d.%m.%Y" currentTime
  putStrLn StringDate
```

Wenn Sie das Programm ausführen, sieht die Ausgabe je nach aktuellem Datum ungefähr so aus:

```Haskell
"28.02.2022"
```

## Tiefe Tauchgang:

### Historischer Kontext

Die Funktion `formatTime` wird in Haskell seit der Version 6.8.2, die im Jahr 2007 veröffentlicht wurde, verwendet. Sie ist Teil des `Data.Time` Moduls, das umfangreiche Funktionen für den Umgang mit Zeit und Datum bietet.

### Alternativen

Eine Alternative zur `formatTime` Funktion wäre, eigene Funktionen zu schreiben, um ein Datum manuell zu formatieren. Das wäre jedoch wahrscheinlich fehleranfälliger und auch weniger praktisch.

### Implementierungsdetails

Die `formatTime` Funktion verwendet im Hintergrund eine monadische Berechnung zur Umwandlung des Datums in einen String. Sie benutzt eine spezielle Syntax, in der Variablen wie `%d`, `%m`, und `%Y` durch den Tag, Monat bzw. das Jahr des Datums ersetzt werden.

## Siehe auch

Weitere Informationen zur `formatTime` Funktion finden Sie in der Haskell Bibliotheksdokumentation: 

[Data.Time.Format-Dokumentation](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)

Eine vollständigere Anleitung zur Behandlung von Datum und Zeit in Haskell finden Sie in diesem Blog-Beitrag: 

[Haskell für Anfänger: Datum und Zeit](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/dates-and-time)