---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:36:25.536740-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String wandelt Text in ein Datumsformat um, das der Computer verstehen und bearbeiten kann. Programmierer tun das, um datumsbezogene Operationen wie Sortierung oder Differenzberechnung zu ermöglichen.

## How to:
In Haskell gebruachen wir die `Time`-Bibliothek, um Datums-Strings zu parsen. Hier ein einfaches Beispiel:

```haskell
import Data.Time.Format (parseTimeM, defaultTimeLocale)

main :: IO ()
main = do
  let format = "%Y-%m-%d"
      dateString = "2023-03-15"
  parseResult <- parseTimeM True defaultTimeLocale format dateString :: IO (Maybe Day)
  case parseResult of
    Just day -> print day
    Nothing -> putStrLn "Das Parsen des Datums ist fehlgeschlagen."

-- Ausgabe:
-- 2023-03-15
```

## Deep Dive
Das Parsen von Datumswerten ist essentiell, da Termine und Fristen oft in textueller Form vorliegen. Die Funktion `parseTimeM` wurde in Haskell entwickelt, um Strings anhand eines angegebenen Formats in Daten umzuwandeln. Historisch gesehen war die Datumsverarbeitung komplizierter, verschiedene Standards haben das heute vereinfacht.

Alternativen zum `Time`-Modul umfassen Pakete wie `chronos` oder `thyme`, die zwar ähnliche Funktionen bieten, aber unterschiedliche Designphilosophien haben. Die `parseTimeM`-Funktion basiert auf Typklassen, was ihre Verwendung typsicher macht. Es ist jedoch wichtig, das korrekte Format zu spezifizieren, da Fehlformatierungen zu einem `Nothing`-Ergebnis führen.

## See Also
- Haskell `Time`-Library: http://hackage.haskell.org/package/time
- Haskell Wiki zu Datums- und Zeitfunktionen: https://wiki.haskell.org/Working_with_time
- Tutorials zu `Data.Time`: https://two-wrongs.com/haskell-time-library-tutorial
