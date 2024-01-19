---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Fish Shell Programmering: Hvordan få tak i dagens dato

## Hva & Hvorfor?

Å få tak i dagens dato innebærer å hente informasjonen om dagens dato og tid. Dette er essensielt for programmerere når de vil spore tidspunktet for bestemte handlinger eller lage tidspesifikke funksjoner i en applikasjon.

## Hvordan gjøre det:

I en Fish Shell, å få dagens dato er ganske enkelt. Her er kodeeksempelet og utdataen:

```Fish Shell
# Få dagens dato
set -l dagensDato (date +"%Y-%m-%d")
echo $dagensDato
```

Output vil være på formatet 'ÅÅÅÅ-MM-DD', for eksempel:

```Fish Shell
2022-12-14
```

## Dypdykk

Historisk sett var tids- og datostyring en kompleks oppgave for tidlige programmerere. Språk som C nødvendig mange linjer kode for å utføre det som nå kan gjøres i en enkelt linje i Fish Shell.

Alternativt kan du i noen tilfeller ønske å bruke (date + "%s") for å hente Unix-tidstempel, som representerer sekundene som har gått siden 1. januar 1970 kl. 00:00 UTC.

Implementeringen av 'date' -kommandoen i Fish Shell stammer fra Unix 'Date' -kommando. Den bringer Unix kraft og fleksibilitet til oppgaven med å hente det nåværende tidspunktet, med den ekstra fordelen av å være enklere å bruke.

## Se også:

For ytterligere lesing og beslektede kilder, se følgende lenker:
- Fish Shell Dokumentasjon: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Unix 'Date' -kommando: [https://man7.org/linux/man-pages/man1/date.1.html](https://man7.org/linux/man-pages/man1/date.1.html)