---
title:                "Å sjekke om en mappe eksisterer"
html_title:           "Fish Shell: Å sjekke om en mappe eksisterer"
simple_title:         "Å sjekke om en mappe eksisterer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor
Det er viktig å sjekke om en mappe eksisterer i Fish Shell for å sikre at skriptet ditt kjører ordentlig og unngå feil og avbrudd.

# Hvordan 
For å sjekke om en mappe eksisterer i Fish Shell, kan du bruke kommandoen `test`, etterfulgt av `-d` og navnet på mappen du ønsker å sjekke. Dette vil returnere et svar i form av en boolean, `0` for sant og `1` for usant.

```
Fish Shell> test -d mappenavn 
0
```

Dette betyr at mappen eksisterer og du kan fortsette å kjøre skriptet ditt uten problemer. Hvis svaret er `1`, må du håndtere dette som en feil og sørge for at skriptet ditt håndterer dette scenariet.

# Dypdykk
Det kan være nyttig å vite hva som skjer under overflaten når du bruker `test -d` kommandoen. Denne kommandoen bruker faktisk en built-in funksjon i Fish Shell kalt `string`, som sjekker om en bestemt streng eksisterer, og i dette tilfellet, sjekker om strengen er en mappe.

En annen måte å sjekke om en mappe eksisterer på, er å bruke `begin` og `end` blokker i Fish Shell.

```
Fish Shell> begin
    if test -d mappenavn
        echo "Mappen eksisterer!"
    end
end
```

Dette vil kjøre `test -d` kommandoen og deretter utføre en handling, i dette tilfellet å skrive ut en melding, hvis mappen eksisterer.

# Se også
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Sjekke om en fil eksisterer i Fish Shell](https://fishshell.com/docs/current/cmds/test.html#test-file-exists)