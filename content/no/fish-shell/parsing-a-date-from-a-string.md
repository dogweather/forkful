---
title:                "Analysere en dato fra en streng"
html_title:           "Fish Shell: Analysere en dato fra en streng"
simple_title:         "Analysere en dato fra en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva og hvorfor? 
Når vi programmerer, kan det ofte være nødvendig å hente ut en dato fra en streng (for eksempel en tekststreng eller et brukerinput). Dette kalles å "parse" en dato fra en streng, og det er nyttig for å kunne behandle og manipulere datoen videre. 

## Hvordan: 
For å parse en dato fra en streng i Fish Shell, kan vi bruke kommandoen `date -f` som tar inn et formatargument og en streng som inneholder datoen. Her er et eksempel på hvordan det kan se ut: 
```Fish Shell 
set dato = "2. desember 2021" 
set format = "%d. %B %Y"
echo (date -f $format $dato)
```
Dette vil gi følgende output: `tor. 2. desember 2021` 

## Fordypning: 
Parsing av datoer fra strenger er et vanlig problem for programmerere, spesielt når man håndterer brukerinndata. Det finnes også ulike måter å parse datoer på, avhengig av programmeringsspråket man bruker. I Fish Shell kan man også bruke kommandoen `strptime` for å parse datoer. Denne funksjonen tar inn en streng og et formatargument og gir tilbake en dato. Implementasjonen av parsing av datoer kan være avhengig av systemets lokalinnstillinger. 

## Se også: 
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/date.html)
- [Fish Shell utvidelser](https://fishshell.com/docs/current/tutorial.html#tut_extending) for å utvide funksjonaliteten til Fish Shell, inkludert parsing av datoer.
- [Wikipedia](https://en.wikipedia.org/wiki/Date_format_by_country) for mer informasjon om formatering av datoer etter land.