---
title:                "Sammenligning av to datoer"
html_title:           "Fish Shell: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noensinne har programmert i Fish Shell, har du kanskje hatt behov for å sammenligne to datoer. Dette kan være nyttig når man skal filtrere eller sortere data basert på datoer, eller når man trenger å utføre handlinger på data som befinner seg innenfor en bestemt tidsperiode. I denne artikkelen vil jeg vise deg hvordan du kan sammenligne to datoer ved hjelp av Fish Shell.

## Hvordan
For å sammenligne to datoer i Fish Shell, kan du bruke kommandoen `date -j -f`. La oss si at vi har to datoer, 19. juni 2021 og 25. juni 2021, og ønsker å finne ut om 25. juni er etter 19. juni. Dette kan gjøres ved å bruke følgende kommando:

```Fish Shell
date -j -f "%d.%m.%Y" 19.06.2021 +%s
```
Dette vil konvertere den første datoen til et tall som representerer antall sekunder siden 1. januar 1970. Deretter kan vi gjøre det samme for den andre datoen:

```Fish Shell
date -j -f "%d.%m.%Y" 25.06.2021 +%s
```

Til slutt kan vi sammenligne de to tallene, og hvis resultatet er positivt, vet vi at 25. juni er etter 19. juni.

Du kan også bruke samme metode for å sammenligne klokkeslett i tillegg til datoer.

## Deep Dive
Hvis du ønsker å gå enda dypere i sammenligning av datoer i Fish Shell, finnes det flere metoder du kan bruke. En annen måte å sammenligne datoer på er å bruke `strftime` kommandoen. Denne kommandoen lar deg formatere datoer og klokkeslett på forskjellige måter, og deretter sammenligne dem.

Du kan også bruke `set -l` kommandoen til å lagre datoen som en variabel, og deretter bruke `test` kommandoen til å utføre sammenligningen. Dette kan være nyttig hvis du trenger å sammenligne flere datoer eller klokkeslett i en løkke.

Det finnes også mange plugins tilgjengelig for Fish Shell som tilbyr mer avanserte funksjoner for å sammenligne datoer og klokkeslett. Utforsk gjerne disse hvis du vil ta dypdykk i emnet.

## Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)
- [10 Fish Shell tips og triks](https://www.telegraph.co.uk/men/thinking-man/10-fish-shell-tips-tricks-getting-work-done-command-line/)