---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når utviklere lager programmer, er det viktig å sjekke om en mappe eksisterer før man prøver å jobbe med den. Dette er for å unngå feilmeldinger og problemer som kan oppstå hvis mappen ikke finnes.

## Hvordan:
For å sjekke om en mappe eksisterer i Fish Shell, kan du bruke kommandoen ```test -d [mappe]```. Denne kommandoen vil returnere sann hvis mappen eksisterer, ellers vil den returnere usann. Her er et eksempel:

```
Fish Shell $ test -d Documents
true
```

```
Fish Shell $ test -d NonexistentFolder
false
```

## Dypdykk:
Sjekking av mapper eksisterer ikke bare i Fish Shell, men er en vanlig praksis i ulike programmeringsspråk. Å sjekke om en fil eksisterer, har en lignende syntaks med kommandoen ```test -f [fil]```. Det er også mulig å bruke ```-e``` flagget for å sjekke om både filer og mapper eksisterer.

Det finnes flere alternativer å sjekke om mapper eksisterer, som for eksempel ```[ -d [mappe] ]``` og ```stat -f "%F" [mappe]```. Disse kommandoene har en lignende funksjonalitet, men marginale forskjeller i bruk og resultat.

I Fish Shell er kommandoen ```test``` og flaggene ```-d``` og ```-f``` del av shell syntax, som gjør det enkelt å skrive korte og effektive skript for å sjekke om mapper og filer eksisterer.

## Se også:
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/test.html)
- [Bash Shell: How to tell if a directory exists?](https://stackoverflow.com/questions/59838/how-to-check-if-a-directory-exists-in-a-shell-script)
- [The Test Command](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)