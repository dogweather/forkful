---
title:                "Fish Shell: Utskrift av feilsøkingsutdata"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Når man skriver kode, er det alltid en god idé å inkludere debug-utskrifter i koden for å feilsøke og forstå hvordan koden fungerer. Debug-utskrifter kan også bidra til å forbedre koden din ved å hjelpe deg med å finne feil og ineffektivitet.

## Hvordan gjøre det
Fish Shell tilbyr en enkel måte å skrive ut debug-utskrifter i koden din. For å gjøre dette, bruker du kommandoen `echo` etterfulgt av en beskrivelse av hva du vil skrive ut i anførselstegn. La oss si at du vil skrive ut verdien av en variabel kalt `count`, du kan gjøre det på følgende måte:

```
Fish Shell kode:
set count 5
echo "Verdien av count er $count"
```

Denne koden vil skrive ut `Verdien av count er 5` når den kjøres. Du kan også skrive ut verdier av flere variabler ved å bruke mellomrom mellom hver variabelbeskrivelse i `echo`-kommandoen.

## Dypdykk
Å legge til debug-utskrifter i koden din kan være en uunnværlig måte å forstå og feilsøke koden din på. Ved å se verdier av variabler og utskrifter fra bestemte deler av koden din, kan du identifisere potensielle problemområder og forbedre koden din. Husk at du alltid kan fjerne disse utskriftene når du er ferdig med å feilsøke, så de påvirker ikke ytelsen til koden din.

## Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Hvordan bruke variabler i Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_variables)
- [Debugging i Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_debugging)