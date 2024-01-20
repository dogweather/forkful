---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolering av streng i PowerShell

## Hva & Hvorfor?

Strenginterpolering er en teknikk i programmering der vi kan inneby variabler i en streng direkte. Programmerere gjør dette for å gjøre kode mer lesbar og forenkle formateringsoppgaver.

## Hvordan gjøre det:

I PowerShell er interpolering av streng bekreftet av dobbel anførselstegn(" "). Se nedenfor:

```PowerShell
$navn = "Ola"
$hilsen = "Hei, $navn"
$hilsen
```

Output vil være: 

```
"Hei, Ola"
```

Hvis du bruker enkle anførselstegn, vil det ikke gi det forventede resultatet:

```PowerShell
$hilsen = 'Hei, $navn'
$hilsen
```

Output vil være: 

```
"Hei, $navn"
```

## Dypdykk:

Historisk sett har strenginterpolering vært en del av programmeringsspråk i flere tiår, og den tidligste implementeringen kan dateres tilbake til 1960-tallet.

Det er alternativer til strenginterpolering. En av de mest brukte er å bruke en + operatør for å konvertere og koble objekter til strenger. Men dette kan bli rotete når det er mange variabler involvert.

Hvordan strenginterpolering fungerer i bakgrunnen i PowerShell er ganske enkelt: det leter etter $-tegnet inne i dobbelte anførselstegn, og det som følger etter er ansett som en variabel.

## Se også:

Sjekk ut disse ressursene for mer informasjon:

1. [Microsofts offisielle dokumentasjon om strenginterpolering](https://docs.microsoft.com/powershell/)