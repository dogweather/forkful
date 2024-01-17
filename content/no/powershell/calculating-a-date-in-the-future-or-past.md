---
title:                "Beregning av datoer i fremtiden eller fortiden"
html_title:           "PowerShell: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å beregne en dato i fremtiden eller fortiden betyr å bruke programmeringskoden til å bestemme en dato basert på et visst antall dager, måneder eller år foran eller bak den nåværende datoen. Dette kan være nyttig når man lager tidslinjer eller planlegger fremtidige hendelser. Programmerere gjør dette for å automatisere denne beregningen og spare tid og arbeidskraft.

## Slik gjør du det:
```PowerShell
# Beregn dato i fremtiden
(Get-Date).AddDays(7) # Legger til 7 dager til nåværende dato
(Get-Date).AddMonths(3) # Legger til 3 måneder til nåværende dato
(Get-Date).AddYears(1) # Legger til 1 år til nåværende dato

# Beregn dato i fortiden
(Get-Date).AddDays(-7) # Trekker fra 7 dager fra nåværende dato
(Get-Date).AddMonths(-3) # Trekker fra 3 måneder fra nåværende dato
(Get-Date).AddYears(-1) # Trekker fra 1 år fra nåværende dato

```

Eksempel på utdata:
```
Monday, March 16, 2020 9:00:00 PM
Monday, March 23, 2020 9:00:00 PM
Tuesday, June 16, 2020 9:00:00 PM
```

## Dypdykk:
Å beregne datoer i fremtiden eller fortiden har vært en viktig del av dataprogrammering i mange år. Før var det vanlig å bruke komplekse formler og beregninger for å utføre denne oppgaven, men nå kan det enkelt gjøres ved hjelp av innebygde funksjoner i programmeringsspråk som PowerShell.

Det finnes også andre måter å beregne datoer på, for eksempel å bruke et tredjeparts bibliotek eller å lage sin egen funksjon. Det viktigste er å velge en metode som fungerer best for ditt spesifikke behov.

Implementasjonen av å beregne datoer i fremtiden eller fortiden i PowerShell er basert på å legge til eller trekke fra et gitt antall dager, måneder eller år fra nåværende dato ved hjelp av innebygde funksjoner.

## Se også:
- [Microsofts dokumentasjon om datobevegelser i PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/Add-Days?view=powershell-7)
- [En eksempelkalkulator for beregning av datoer i PowerShell](https://www.experts-exchange.com/articles/3741/PowerShell-extension-tool-wanted-anybody-got-one.html)