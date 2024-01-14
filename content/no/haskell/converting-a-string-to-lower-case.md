---
title:    "Haskell: Konvertering av en streng til små bokstaver"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Skal vi virkelig diskutere hvorfor vi skulle ønske å konvertere en streng til små bokstaver? Vel, det kan være ulike årsaker til det, men det er vanligvis for å gjøre data mer konsistente og enklere å behandle. For eksempel, hvis du har en liste med navn og noen av dem er skrevet med store bokstaver, mens andre er i små bokstaver, kan det være ønskelig å konvertere alle navnene til samme format for å unngå forvirring. I tillegg kan ulike funksjoner og algoritmer ha ulike krav til hvordan input data skal være formatert, og derfor kan det være nyttig å konvertere strengen til et format som passer best for bruken.

## Hvordan

Å konvertere en streng til små bokstaver i Haskell er enkelt, takket være den innebygde funksjonen "map". La oss se på en enkel implementering:

```Haskell
import Data.Char (toLower) -- importerer funksjonen "toLower" fra Data.Char-modulen

konverterTilSmåBokstaver :: String -> String -- funksjon som tar inn en streng som input og returnerer en streng
konverterTilSmåBokstaver = map toLower -- bruker funksjonen "map" og "toLower" på hver bokstav i strengen
```

La oss prøve det ut med noen eksempler og se hva som skjer:

```Haskell
konverterTilSmåBokstaver "Hei på deg!" -- output: "hei på deg!"
konverterTilSmåBokstaver "Even og Maria" -- output: "even og maria"
```

Som vi kan se, blir alle bokstavene omgjort til små bokstaver, inkludert æ, ø og å.

## Dypdykk

Det er verdt å merke seg at funksjonen "toLower" kun konverterer engelske bokstaver til små bokstaver. For å konvertere andre språk sine bokstaver, må man bruke en annen funksjon som er spesifikk for det ønskede språket. I tillegg kan det være utfordrende å konvertere tegnsett som ikke er en del av det latinske alfabetet, og det kan være nødvendig å bruke eksterne biblioteker eller implementere egne funksjoner for å håndtere disse tilfellene.

## Se også

- [Haskell Data.Char-modulen](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [Offisiell Haskell-dokumentasjon](https://www.haskell.org/documentation/)
- [Enkel guide til Haskell](https://www.codementor.io/@jhanson701/haskell-functional-programming-git-getting-started-vyrg151ce)