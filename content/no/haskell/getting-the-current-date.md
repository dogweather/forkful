---
title:    "Haskell: Å få gjeldende dato"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor
En av de mest grunnleggende operasjonene i programmering er å kunne håndtere datoer og klokkeslett. Uansett hva slags applikasjon du utvikler, vil du mest sannsynlig trenger å vite dagens dato. Derfor er det viktig å lære hvordan man kan hente og håndtere datoer i Haskell.

## Hvordan
For å få den gjeldende datoen i Haskell, kan du bruke funksjonen `getCurrentTime` fra modulen `Data.Time`. Her er et eksempel på hvordan du kan bruke denne funksjonen for å hente datoen og skrive den ut på konsollen:

```Haskell
import Data.Time

main = do
    currentTime <- getCurrentTime
    print $ show $ utctDay currentTime
```

Dette vil skrive ut datoen i formatet `YYYY-MM-DD`. Du kan også bruke andre funksjoner fra `Data.Time` for å formatere datoen på ønsket måte.

## Dypdykk
Nå som du vet hvordan du kan få tak i datoen, kan du også dykke dypere inn i hvordan datoen og tidsinformasjonen er representert i Haskell. Dato og klokkeslett er representert ved hjelp av datatypen `UTCTime`, som er en del av modulen `Data.Time.Clock`. Denne datatypen inneholder informasjon om år, måned, dag, time, minutt, sekund og tidssone. Du kan bruke funksjoner som `utctDay` og `utctDayTime` for å hente ut spesifikke deler av datoen.

## Se også
- [Data.Time dokumentasjon](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell tutorials](https://www.haskell.org/tutorial/index.html)
- [Hva er Haskell?](https://www.haskell.org/about.html)