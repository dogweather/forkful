---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Es geht darum, das aktuelle Datum zu finden. Programmierer tun dies, weil es hilfreich ist, wenn sie zeitabhängige Aktionen durchführen oder Ereignisse protokollieren müssen.

## So geht's:
Mit Haskell können Sie das aktuelle Datum mit der Unterstützung von `Data.Time` Bibliothek finden.

```haskell
import Data.Time

main = do
  aktuellesDatum <- getCurrentTime
  print aktuellesDatum
```

Wenn Sie dieses Programm ausführen, erhalten Sie eine Ausgabe ähnlich wie diese:

```haskell
2022-03-25 12:50:05.892951 UTC
```

Die Ausgabe repräsentiert das aktuelle Datum und die aktuelle Uhrzeit in UTC.

## Vertiefung:

(1) Historischer Kontext: Die Funktion `getCurrentTime` ist ein Teil der Zeitbibliothek, die in Haskell seit seiner Version 6.6 vorhanden ist.

(2) Alternativen: Es gibt andere Methoden, das Datum zu bekommen, wie etwa der Einsatz der `System.Locale` und `System.Time` Bibliotheken.

(3) Implementierungsdetails: `getCurrentTime` gibt ein `IO UTCTime` zurück. Haskell verwendet den Typ `UTCTime`, um den unabhängigen Zeitpunkt auszudrücken. Die Angabe erfolgt dabei in Koordinierter Weltzeit (UTC).

## Siehe auch:

Besuchen Sie die offizielle Haskell-Dokumentation für weitere Details:

- [Data.Time Docs](https://hackage.haskell.org/package/time-1.12/docs/Data-Time.html)
- [System.Locale Docs](https://hackage.haskell.org/package/old-locale-1.0.0.7/docs/System-Locale.html)
- [System.Time Docs](https://hackage.haskell.org/package/old-time-1.1.0.3/docs/System-Time.html)