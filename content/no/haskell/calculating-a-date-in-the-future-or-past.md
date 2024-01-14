---
title:    "Haskell: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Markdown, den nye standarden for å formatere tekst til web, har blitt stadig mer populær innenfor programmering. Den har blitt et foretrukket valg for å skrive dokumentasjon, readme-filer og blogginnlegg. Hvis du er på utkikk etter et robust og elegant programmeringsspråk for å fremstille datostempler, bør du ikke lete lenger enn Haskell.

## Hvorfor

Det kan være mange grunner til å ville beregne en dato i fortid eller fremtid. Kanskje du ønsker å lage en funksjon som kan forutsi din neste bursdag, eller kanskje du trenger å beregne leveringsdatoen for en bestilling. Uansett hva årsaken er, er Haskell et flott valg for å utføre slike beregninger.

## Hvordan

Vi kan bruke Haskell-funksjonen `addDays` for å legge til et gitt antall dager til en dato. For eksempel, hvis vi ønsker å finne datoen 100 dager fra nå, kan vi skrive følgende kode:

```Haskell
addDays 100 (fromGregorian 2021 1 1)
```

Dette vil gi oss datoen 11. april 2021. Vi kan også bruke `addDays (-100)` for å finne datoen 100 dager tilbake i tid.

En annen nyttig funksjon er `diffDays`, som lar oss finne antall dager mellom to datoer. For eksempel, hvis vi ønsker å finne hvor mange dager det er mellom 1. januar 2021 og 1. mars 2021, kan vi skrive følgende kode:

```Haskell
diffDays (fromGregorian 2021 3 1) (fromGregorian 2021 1 1)
```
Dette vil gi oss svaret 59 dager.

## Dypdykk

Haskell har også mer avanserte funksjoner for å beregne datoer, som for eksempel `addGregorianMonthsRollOver` og `addGregorianYearsRollOver`. Disse tar hensyn til ulike faktorer som skuddår og antall dager i måneden, og sørger for at vi får riktig resultat uansett hvilken dato vi starter med.

En ting å merke seg er at Haskell bruker Gregorian-kalenderen for å beregne datoer, så resultatene kan variere fra andre kalendere. Det kan også være lurt å være forsiktig med å bruke disse funksjonene for svært lange tidsperioder, da de kan være unøyaktige på grunn av variasjoner i månedslengder over tid.

## Se Også

- [Offisiell Haskell-nettside](https://www.haskell.org/)
- [Haskell Tutorial](https://www.tutorialspoint.com/haskell/)
- [Haskell-funksjoner for datoer](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html#g:3)
- [Markdown-dokumentasjon](https://www.markdownguide.org/)