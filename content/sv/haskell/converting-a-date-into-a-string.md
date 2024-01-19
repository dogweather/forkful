---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hur man omvandlar ett datum till en sträng i Haskell

## Vad & Varför?

Omvandling av ett datum till en sträng i programmering innebär att ta ett datumobjekt och skapa en läsbar textrepresentation av det. Programmerare gör detta för att organisera, presentera eller logga information på ett mänskligt läsbar sätt.

## Hur man gör:

I Haskell kan du konvertera ett datum till en sträng med hjälp av biblioteket "Data.Time" och dess metoder. Här är ett exempel:

```Haskell
import Data.Time

main = do
    getCurrentTime >>= print . formatTime defaultTimeLocale "%Y-%m-%d"
```

När detta kodexempel körs, returnerar det dagens datum som en sträng i formatet "ÅÅÅÅ-MM-DD".

## Djupdykning:

1. Historiskt sammanhang: Haskell, lanserad 1990, är känt för dess lathet, stark statiska typning, och ren funktionell programmeringssyntax. Användningen av "`defaultTimeLocale`" vi ser i exemplet kommer från `Data.Time.Format.Locale`, som innehåller specifikationer för hur tider och datum ska formateras och tolkas i olika kulturer och språk. 
   
2. Alternativt: Ett annat sätt att konvertera ett datum till en sträng i Haskell är att använda "show" -funktionen. Observera dock att använda "show" direkt på ett datumobjekt kan ge resultat som kanske inte är i det önskade formatet.
   
3. Implementeringsdetaljer: Funktionen "`formatTime`" tar en locale (här "`defaultTimeLocale`"), en formatsträng och ett tidpunkt. Den returnerar tidpunkten formaterad enligt formatsträngen. 

## Se Också:

1. Haskell's Data.Time Library Documentation: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
2. En Guide till DateTime i Haskell: https://williamyaoh.com/posts/2019-09-16-a-cheatsheet-to-json-time.html
3. Haskell Documentation om formatTime och andra datumrelaterade funktioner: http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Format.html.
4. Visible Programmering på Haskell: http://learnyouahaskell.com/starting-out#ready-set-go

Nu har du kunskapen. Lycka till med din Haskell-programmering!