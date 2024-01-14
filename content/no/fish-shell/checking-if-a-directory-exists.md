---
title:    "Fish Shell: Å sjekke om en mappe eksisterer"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sjekke om en mappe eksisterer er en viktig og vanlig prosess når man skrive programmer i Fish Shell. Dette kan være nyttig for å unngå feil og sikre at koden din fungerer som forventet. 

# Hvordan gjøre det

For å sjekke om en mappe eksisterer i Fish Shell, kan vi bruke kommandoen `test -d`. Denne kommandoen vil returnere en sann/ulik-verdi (true/false) basert på om mappen eksisterer eller ikke. Her er et eksempel på hvordan du kan bruke denne kommandoen:

```Fish Shell
test -d Documents
```

Hvis mappen "Documents" eksisterer, vil denne kommandoen returnere sann (true). Hvis ikke, vil det returnere ulikt (false).

En annen måte å sjekke om en mappe eksisterer på er å bruke kommandoen `exists`. Denne kommandoen vil også returnere en sann/ulik-verdi basert på om mappen eksisterer eller ikke. Her er et eksempel på hvordan du kan bruke denne kommandoen:

```Fish Shell
exists Pictures
```

I tillegg til disse kommandoene, kan du også bruke forskjellige parametere som `-L` for å sjekke om en mappe er en symbolisk lenke, eller `-P` for å sjekke om en mappe er en vanlig mappe og ikke en lenke. Du kan lese mer om disse parametrene ved å bruke kommandoen `man test` i terminalen.

# Dypdykk

Under overflaten bruker `test -d` og `exists` kommandoene faktisk `stat` kommandoen for å sjekke om en mappe eksisterer. `stat` kommandoen viser statusinformasjon om en fil eller mappe, inkludert om den eksisterer eller ikke. Når vi bruker `test -d` eller `exists` i Fish Shell, bruker de denne informasjonen til å returnere en sann/ulik-verdi basert på om mappen eksisterer eller ikke. 

Det er også verdt å merke seg at `test -d` og `exists` kommandoene også vil fungere for å sjekke om en fil eksisterer, ved å bruke filens fulle bane i stedet for mappenavn. 

# Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Linux-kommandoen "test"](https://linux.die.net/man/1/test)
- [Linux-kommandoen "stat"](https://linux.die.net/man/1/stat)