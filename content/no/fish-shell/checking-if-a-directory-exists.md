---
title:    "Fish Shell: Sjekker om en mappe eksisterer"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# Hvorfor

Hvis du noen gang har jobbet med å programmere i Fish Shell, har du kanskje lurt på om det er en enkel måte å sjekke om en mappe eksisterer. Å kjenne til denne funksjonaliteten kan være nyttig for å sørge for at skriptene dine fungerer som de skal og for å unngå feil.

# Slik gjør du det

Det er en enkel kommando i Fish Shell for å sjekke om en mappe eksisterer, nemlig `test -d /sti/til/mappe`. Denne kommandoen tester om en mappe finnes på den spesifiserte plasseringen og vil returnere en sann eller usann verdi. Her er et eksempel på hvordan dette kan se ut i praksis:

```Fish Shell
test -d /home/bruker/dokumenter

echo $status
```

I dette eksempelet vil `echo $status` printe ut enten `0` (sant) hvis mappen eksisterer, eller `1` (usant) hvis den ikke gjør det. På denne måten kan du lage en betingelse i skriptet ditt basert på resultatet av kommandoen.

# Dypdykk

En annen måte å sjekke om en mappe eksisterer er å bruke `stat` kommandoen. Denne kommandoen vil gi deg informasjon om en fil eller mappe, inkludert størrelse, modus og når den sist ble endret. For å sjekke om en mappe eksisterer, kan du bruke `stat -t /sti/til/mappe` og sjekke om kommandoen returnerer en feilmelding. Dette kan være nyttig hvis du ønsker å få mer informasjon om mappen i tillegg til å sjekke om den eksisterer.

# Se også

- [Fish Shell offisiell nettside](https://fishshell.com/)
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)