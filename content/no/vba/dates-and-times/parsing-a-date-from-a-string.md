---
title:                "Analysering av en dato fra en streng"
aliases:
- /no/vba/parsing-a-date-from-a-string.md
date:                  2024-02-01T21:57:44.514908-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av en dato fra en streng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/vba/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å analysere en dato fra en streng i Visual Basic for Applications (VBA) handler om å konvertere tekst som representerer en dato til en datatotype for dato. Programmerere gjør dette for å kunne manipulere datoer mer effektivt i applikasjonene sine, for eksempel til sammenligninger, beregninger eller formateringsformål.

## Hvordan:

VBA tilbyr en enkel måte å analysere en streng til en dato ved bruk av `CDate`-funksjonen eller `DateValue`-funksjonen. Det er imidlertid avgjørende at strengen er i et gjenkjennelig datoformat.

Her er et grunnleggende eksempel som bruker `CDate`:

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Analysert Dato: "; parsedDate
End Sub
```

Hvis du kjører denne koden, vil utskriften i Det umiddelbare vinduet (tilgjengelig via `Ctrl+G` i VBA-editoren) være:

```
Analysert Dato: 4/1/2023 
```

Alternativt kan du bruke `DateValue`-funksjonen, som er mer spesifikk for datoer (ignorerer tidsdelen):

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "April 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Analysert Dato med DateValue: "; parsedDate
End Sub
```

Eksempelutskrift for dette vil på samme måte vise i Det umiddelbare vinduet:

```
Analysert Dato med DateValue: 4/1/2023
```

Ha i tankene at suksessen med analyse avhenger av at datoformatet til strengen matcher system- eller applikasjonsinnstillingene.

## Dypdykk

Internt, når VBA analyserer en streng til en dato, bruker det de regionale innstillingene til Windows-operativsystemet for å tolke datoformatet. Dette er avgjørende å forstå fordi en datostreng som perfekt analyseres på ett system kanskje forårsaker en feil på et annet dersom de bruker ulike dato-/tidsinnstillinger.

Historisk sett har håndtering av datoer vært en vanlig kilde til feil i applikasjoner, spesielt de som brukes internasjonalt. Denne avhengigheten av regionale innstillinger i VBA er grunnen til at noen kan vurdere alternativer som ISO 8601-formatet (f.eks., "ÅÅÅÅ-MM-DD") for entydig datorepresentasjon og analyse på tvers av ulike systemer. Dessverre støtter ikke VBA nativt ISO 8601, og manuell analyse ville vært nødvendig for streng overholdelse.

For kompleks datoanalyse utover hva `CDate` eller `DateValue` kan håndtere, eller for å sikre konsistent analyse uavhengig av systemlokale innstillinger, kan programmerere ty til egendefinerte analysefunksjoner. Disse kunne involvere å splitte datostrengen i komponenter (år, måned, dag) og konstruere en dato ved hjelp av `DateSerial`-funksjonen. Andre kan velge kraftigere språk eller biblioteker designet med internasjonalisering i tankene for slike oppgaver.
