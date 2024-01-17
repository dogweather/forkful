---
title:                "Få nåværende dato"
html_title:           "Fish Shell: Få nåværende dato"
simple_title:         "Få nåværende dato"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å få den nåværende datoen er en viktig funksjon for programmerere, da den lar dem hente og vise den aktuelle datoen under kjøring. Dette kan være nyttig for å organisere og presentere informasjon på en relevant måte.

# Hvordan:

Fish Shell har en innebygd kommando,```date```, for å hente den nåværende datoen. Dette kan gjøres ved å følge disse enkle trinnene:

1. Åpne Fish Shell-terminalen.
2. Skriv inn ```date``` og trykk Enter.
3. Du vil nå se den nåværende datoen i formatet "Day Month Date Hour:Minute:Second Timezone Year".

Eksempel output:

``` 
Fri 19 Feb 16:50:09 CET 2021
```

# Dypdykk:

Funksjonen for å få den nåværende datoen er ikke begrenset til Fish Shell, men er tilgjengelig i de fleste skall (shells). Dette inkluderer BASH, CSH og ZSH. I tillegg, for å få en mer spesifikk datoformat, kan du bruke flagg som ```date -r``` for å vise dato og klokkeslett for en spesifikk fil og ```date +%B``` for å få kun månedsnavnet.

# Se også:

For mer informasjon om å få den nåværende datoen i Fish Shell, sjekk ut dokumentasjonen på [Fish Shell-nettstedet](https://fishshell.com/docs/current/commands.html#date). Du kan også se [date-kommandoen i Linux manualen](http://man7.org/linux/man-pages/man1/date.1.html).