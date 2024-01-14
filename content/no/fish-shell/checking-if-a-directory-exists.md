---
title:                "Fish Shell: Å sjekke om en mappe eksisterer"
simple_title:         "Å sjekke om en mappe eksisterer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor sjekke om en mappe eksisterer i Fish Shell

Når man jobber med programmering, kan det være nyttig å sjekke om en mappe eksisterer før man gjør operasjoner som å opprette eller fjerne filer i den. Dette kan hjelpe til med å unngå feil og sikre at koden kjører som forventet. I denne bloggposten vil vi lære hvordan man kan sjekke om en mappe eksisterer i Fish Shell.

## Slik gjør du det

Det første vi må gjøre er å åpne Fish Shell og navigere til mappen vi ønsker å sjekke. Deretter kan vi bruke kommandoen `test -d` sammen med mappenavnet for å sjekke om mappen eksisterer. Her er et eksempel på hvordan dette kan se ut:

```Fish Shell
test -d Documents # Sjekker om mappen "Documents" eksisterer
```

Dette vil returnere enten `true` eller `false`, avhengig av om mappen eksisterer eller ikke. For å gjøre dette litt mer dynamisk, kan vi bruke en variabel i stedet for et statisk mappenavn:

```Fish Shell
set directory "Documents"
test -d $directory # Sjekker om mappen definert i variabelen eksisterer
```

Vi kan også bruke en logisk operator, for eksempel `&&`, for å utføre en handling basert på om mappen eksisterer eller ikke. Her er et eksempel der vi printer ut en melding hvis mappen eksisterer:

```Fish Shell
test -d Documents && echo "Mappen finnes allerede"
```

## Dykk dypere

Hvis du ønsker å forstå mer om hvordan Fish Shell sjekker om en mappe eksisterer, kan vi dykke litt dypere inn i koden. Kommandoen `test -d` er faktisk en forkortelse for `test -z DIRECTORY`, der `-z` betyr "empty" og sjekker om en variabel er tom eller ikke. Når vi bruker `-d`, oversettes det til `fish_is_directory`.

Fish Shell bruker to hovedkilder for å sjekke om en mappe eksisterer. Den første er en funksjon kalt `fish_working_directory`, som returnerer den nåværende arbeidsmappen. Den andre er en funksjon kalt `fish_complete` som sjekker om en mappe allerede er fullført i Fish sin autocomplete-funksjon. Hvis mappen blir funnet i en av disse to funksjonene, returneres `true`.

## Se også
- [Fish Shell offisiell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Guide til Bash til Fish konvertering](https://fishshell.com/docs/current/tutorial.html)
- [GitHub-siden for Fish Shell](https://github.com/fish-shell/fish-shell)