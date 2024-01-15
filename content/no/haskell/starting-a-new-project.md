---
title:                "Å starte et nytt prosjekt"
html_title:           "Haskell: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bør du vurdere å starte et nytt Haskell-prosjekt? Vel, for det første er Haskell et programmeringsspråk som er kjent for å være åpent og fleksibelt, med bruk av funksjonell programmering. Dette gir deg muligheten til å løse komplekse problemer på en effektiv og elegant måte. Videre har Haskell en sterk og stabil brukerbase, og er et svært nyttig verktøy for å løse ulike problemer innenfor dataanalyse, kunstig intelligens og webutvikling.

## Hvordan

La oss se nærmere på hvordan du kan starte et nytt Haskell-prosjekt. Først må du sørge for at du har GHC (Glasgow Haskell Compiler) installert på datamaskinen din. GHC er nødvendig for å kompilere og kjøre Haskell-kode. Deretter kan du åpne din foretrukne kodeeditor og følge disse stegene:

``` Haskell
module Main where

main :: IO ()
main = do
  putStrLn "Velkommen til ditt første Haskell-prosjekt!"
  putStrLn "Skriv noe kreativt med funksjonell programmering!"
```

Dette er et veldig enkelt eksempel som bare skriver ut to linjer med tekst når du kjører programmet. Men dette viser det grunnleggende strukturen i Haskell-kode - en modul med en `main` funksjon som kjører IO (input/output) handlinger.

For å kjøre dette programmet, må vi kompilere det og deretter kjøre den kompilerte filen. Dette kan gjøres ved å åpne en terminal og navigere til direktoriet hvor filen er lagret. Deretter kan du kjøre følgende kommandoer:

```bash
ghc --make main.hs # kompilerer filen main.hs og lager en kjørbar fil med navnet "main"
./main # kjører den kompilerte filen
```

Etter å ha kjørt dette kommandoen, bør du se følgende utskrift på skjermen din:

```
Velkommen til ditt første Haskell-prosjekt!
Skriv noe kreativt med funksjonell programmering!
```

Gratulerer, du har nå kjørt ditt første Haskell-program!

## Dypdykk

Når du nå har en god forståelse av hvordan du kan sette opp et nytt Haskell-prosjekt, kan vi se på noen flere aspekter ved å starte et nytt prosjekt. Et av de viktigste valgene du må ta i begynnelsen er å velge riktig Haskell-rammeverk for prosjektet ditt. Det finnes mange populære rammeverk, som for eksempel Yesod og Snap, som hjelper deg med å bygge webapplikasjoner i Haskell.

Det er også viktig å forstå konseptet med pakker og avhengigheter i Haskell. Haskell har et kraftig pakkesystem og en sentral pakkeindeks som heter Hackage. Her kan du finne en mengde forskjellige pakker som kan hjelpe deg med å løse ulike problemer og oppgaver.

Til slutt, det er viktig å huske på at Haskell er et funksjonelt programmeringsspråk, og det kan ta litt tid å tilpasse seg en ny måte å tenke på når du programmerer. Men etter hvert vil du oppleve at Haskell gir deg muligheten til å skrive kraftig og effektiv kode på en helt ny måte.

## Se også

- [Offisiell Haskell-nettside](https://www.haskell.org/)
- [Haskell på Wikibooks](https://en.wikibooks.org/wiki/Haskell)
- [Haskell på Hackage](https://hackage.haskell.org/)