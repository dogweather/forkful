---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:40.564668-07:00
description: "\xC5 beregne en dato i fremtiden eller fortiden inneb\xE6rer \xE5 bestemme\
  \ en dato som er et spesifikt antall dager, m\xE5neder eller \xE5r fra en gitt dato.\u2026"
lastmod: '2024-03-13T22:44:40.635413-06:00'
model: gpt-4-0125-preview
summary: "\xC5 beregne en dato i fremtiden eller fortiden inneb\xE6rer \xE5 bestemme\
  \ en dato som er et spesifikt antall dager, m\xE5neder eller \xE5r fra en gitt dato."
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden innebærer å bestemme en dato som er et spesifikt antall dager, måneder eller år fra en gitt dato. Programmerere trenger ofte denne funksjonaliteten for å automatisere påminnelser, abonnementer, utløpsdatoer og planleggingsoppgaver i ulike applikasjoner.

## Hvordan:
I Visual Basic for Applications (VBA) er den primære funksjonen som brukes til å beregne fremtidige eller fortidige datoer `DateAdd()`. Denne funksjonen legger til et spesifisert tidsintervall til en dato, og returnerer en ny dato.

Her er et grunnleggende eksempel for å legge til 10 dager til dagens dato:

```vb
Dim fremtidigDato As Date
fremtidigDato = DateAdd("d", 10, Date) ' Legger til 10 dager til dagens dato
Debug.Print fremtidigDato ' Gir ut noe som: 04/20/2023
```

På samme måte, for å finne en dato 10 dager i fortiden:

```vb
Dim fortidDato As Date
fortidDato = DateAdd("d", -10, Date) ' Trekker fra 10 dager fra dagens dato
Debug.Print fortidDato ' Gir ut: 03/31/2023, antar at i dag er 04/10/2023
```

Disse eksemplene er ganske greie. Du kan erstatte `"d"` med andre intervallkoder, som `"m"` for måneder og `"yyyy"` for år, for å beregne ulike typer datoberegninger. Slik kan du kanskje beregne en dato ett år i fremtiden:

```vb
Dim nesteÅr As Date
nesteÅr = DateAdd("yyyy", 1, Date) ' Legger til 1 år til dagens dato
Debug.Print nesteÅr ' Gir ut: 04/10/2024 hvis i dag er 04/10/2023
```

## Dypdykk
`DateAdd`-funksjonen har vært en grunnleggende del av VBA siden begynnelsen, og stammer fra sin forgjenger BASIC. Selv om den tilbyr enkelhet for å legge til eller trekke fra tidsintervaller fra datoer, er det viktig å merke seg at VBA, inkludert dets datohåndteringsfunksjoner, ikke alltid kan matche bekvemmeligheten eller effektiviteten som finnes i nyere programmeringsspråk.

For eksempel tilbyr moderne språk som Python med `datetime`-modulen eller JavaScript med biblioteker som `moment.js` og `date-fns`, mer intuitive og kraftfulle måter for datomanipulering. Disse alternativene gir bedre støtte for lokalisering, tidssoner og skuddår, noe som kan gjøre dem mer egnet for applikasjoner som krever presis datoberegning på en global skala.

Men, for Excel-makroer og applikasjoner som krever integrering innen Microsoft Office-økosystemet, forblir VBA et praktisk valg. Enkelheten ved å direkte få tilgang til og manipulere Excel-data er en betydelig fordel. Videre, for de fleste grunnleggende datoberegninger som planlegging og påminnelser, gir `DateAdd()` i VBA en adekvat og grei løsning. Dens syntaks er lett å forstå for nykommere, mens integreringen i de bredere Office-suite-applikasjonene sikrer dens relevans i spesifikke bruksområder.

I konklusjonen, selv om alternative programmeringsspråk kan tilby mer moderne tilnærminger til datoberegning, fungerer `DateAdd()` i VBA som et vitnesbyrd om språkets varige kraft i domenene der det er mest nødvendig.
