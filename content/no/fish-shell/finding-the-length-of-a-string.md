---
title:                "Å finne lengden på en streng"
html_title:           "Fish Shell: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en tekststreng kan være nyttig for å kunne behandle og manipulere data på en mer effektiv måte. Ved å vite lengden til en streng, kan vi blant annet bestemme hvor mange ganger en løkke skal kjøre eller hvor mange tegn som skal hentes ut fra en streng.

## Hvordan

Vi kan bruke Fish Shell sin innebygde kommando `string length` for å finne lengden til en streng. Denne kommandoen tar en streng som argument og returnerer antall tegn i strengen.

```Fish Shell
string length "Hei verden"
```
Output:
```
11
```

Vi kan også bruke variabler og funksjoner sammen med `string length` for å få mer fleksibilitet. For eksempel, hvis vi ønsker å finne lengden til en tekststreng som brukeren skriver inn, kan vi bruke følgende kode:

```Fish Shell
set input (read -l "Skriv inn en tekststreng: ")
string length $input
```
Output:
```
12
```

## Dypdykk

Det er verdt å merke seg at `string length` kun teller antall tegn i en streng og ikke antall ord. Derfor, hvis vi ønsker å finne antall ord i en streng, må vi først bruke `string split` for å dele strengen inn i en liste av ord, og deretter bruke `count` funksjonen for å telle antall elementer i lista.

Det er også viktig å være oppmerksom på at `string length` tar hensyn til alle tegn, inkludert mellomrom og spesialtegn. Dette kan påvirke resultatet hvis vi for eksempel har en tekststreng med blandede tegn og ønsker å finne lengden til bare bokstavene.

## Se også

- [Fish Shell sin offisielle dokumentasjon](https://fishshell.com/docs/current/commands.html)
- [Tutorial: How to Use Fish Shell to Automate Tasks on a VPS](https://www.digitalocean.com/community/tutorials/how-to-use-fish-shell-to-automate-tasks-on-a-vps)
- [10 Useful Tips for Fish Shell](https://computingforgeeks.com/10-useful-tips-for-fish-shell-on-linux-macos/)