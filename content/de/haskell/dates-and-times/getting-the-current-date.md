---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:27.093385-07:00
description: "Das Abrufen des aktuellen Datums in Haskell umfasst das Erlangen der\
  \ aktuellen Systemzeit und deren Umwandlung in ein lesbares Datumsformat. Programmierer\u2026"
lastmod: 2024-02-19 22:05:12.868780
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in Haskell umfasst das Erlangen der aktuellen\
  \ Systemzeit und deren Umwandlung in ein lesbares Datumsformat. Programmierer\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in Haskell umfasst das Erlangen der aktuellen Systemzeit und deren Umwandlung in ein lesbares Datumsformat. Programmierer tun dies, um Operationen basierend auf dem Datum durchzuführen, wie zum Beispiel das Protokollieren, das Planen von Aufgaben oder das Zeitstempeln von Ereignissen in Anwendungen.

## Wie geht das:
Die Standardbibliothek von Haskell, `base`, bietet das Modul `Data.Time`, das Funktionalitäten zur Arbeit mit Daten und Zeiten bietet. So verwenden Sie es, um das aktuelle Datum zu erhalten:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

Beispielausgabe:
```
2023-04-12
```

Für mehr Flexibilität, wie zum Beispiel das Formatieren des Datums oder die Arbeit mit verschiedenen Zeitzonen, ist die `time` Bibliothek unschätzbar. Hier sehen Sie, wie Sie das aktuelle Datum formatieren könnten:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

Dies gibt das aktuelle Datum im `YYYY-MM-DD` Format aus, angepasst an die lokale Zeitzone.

Zusätzlich, für Unterstützung durch Drittanbieterbibliotheken, wird `time` aufgrund seiner umfassenden Fähigkeiten zur Manipulation von Datum und Zeit in der Haskell-Community sehr empfohlen und oft verwendet. Die oben genannten Beispiele nutzen diese Bibliothek.

Wenn Sie umfassendere Datummanipulationen benötigen, einschließlich des Parsens von Strings oder arithmetischen Operationen mit Daten und Zeiten, wird das Erkunden zusätzlicher Funktionen innerhalb von `Data.Time` von Vorteil sein.
