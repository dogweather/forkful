---
title:                "Das aktuelle Datum abrufen"
html_title:           "Haskell: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Wenn du dich für die Programmiersprache Haskell interessierst und mehr darüber lernen möchtest, ist das Lesen dieses Artikels das Richtige für dich. Hier erfährst du, wie du das aktuelle Datum in Haskell abrufen kannst. Dadurch kannst du deine Programmierkenntnisse erweitern und nützliche Funktionen in deine Projekte integrieren.

## Wie geht das?

Um das aktuelle Datum in Haskell abzurufen, verwenden wir die Funktion `getCurrentTime` aus dem Modul `Data.Time`.

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  print currentTime
```

Dieser Code importiert das `Data.Time` Modul und ruft die Funktion `getCurrentTime` auf. Das aktuelle Datum wird in der Variable `currentTime` gespeichert und mit `print` ausgegeben.

Die Ausgabe wird in folgendem Format angezeigt:

```
2021-09-25 12:00:00 UTC
```

Du kannst auch das Paket `time` installieren, um das Datum in einem bestimmten Format auszugeben:

```Haskell
import Data.Time.Format

main = do
  currentTime <- getCurrentTime
  print $ formatTime defaultTimeLocale "%B %e, %Y" currentTime
```

Die Ausgabe wird in diesem Format angezeigt:

```
September 25, 2021
```

## Tiefergehende Erklärung

Das `Data.Time` Modul bietet viele nützliche Funktionen für die Arbeit mit Datum und Zeit in Haskell. Die `getCurrentTime` Funktion ruft die aktuelle Zeit aus dem System ab und gibt sie als `UTCTime` zurück. Diese Datenstruktur enthält Informationen wie Jahr, Monat, Tag, Uhrzeit und Zeitzone.

Um das Datum oder die Uhrzeit in einem anderen Format auszugeben, können verschiedene Funktionen aus dem `Data.Time.Format` Modul verwendet werden. Die `formatTime` Funktion akzeptiert ein Formatierungsspezifikation und eine `UTCTime` und gibt das Datum in diesem Format zurück.

## Siehe auch

- [Haskell Dokumentation: Modul Data.Time](https://hackage.haskell.org/package/time-1.10.0.0/docs/Data-Time.html)
- [Haskell Couch: Datum und Uhrzeit](https://www.haskell.org/couch/node/351)