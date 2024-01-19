---
title:                "Sjekker om en katalog eksisterer"
html_title:           "Bash: Sjekker om en katalog eksisterer"
simple_title:         "Sjekker om en katalog eksisterer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sjekke om en katalog eksisterer, innebærer å undersøke om det er en mappe på det gitte stedet. Programmerere gjør dette for å forhindre feil ved forsøk på å aksessere/bruke en ikke-eksisterende katalog.

## Hvordan Du Gjør Det:

Det er en enkel en-linje kommando for å sjekke om en katalog eksisterer i Bash. Her er et eksempel:

```Bash
if [ -d "/sti/til/mappen" ]; then echo "Mappen eksisterer."; else echo "Mappen eksisterer ikke."; fi
```

Hvis mappen eksisterer, vil denne kommandoen uttrykke "Mappen eksisterer.". Hvis ikke, vil den si "Mappen eksisterer ikke.".

## Dypdykk

Checking om en katalog eksisterer ble introdusert tidlig i UNIX OS utviklingen. Unix ‘test’-kommandoen, [ utformet som [ -d DIR ] i vårt tilfelle, ble lagt til i versjon 7 (1979).

Alternativt, hvis du skriver skript for et nyere system, kan du bruke 'dir' i stedet for '[ -d DIR ]'. Men støtte for 'dir' er ikke universell, så '[ -d DIR ]' vil fungere på flere systemer.

Detaljer av implementeringen involverer hvordan 'test'-kommandoen bruker systemkall til operativsystemet for å forespørsel informasjon om filsystemet. Det er en rask og effektiv operasjon.

## Se Også

For mer diskusjon og eksempler på testing for filer og kataloger i shell scripts, se:
- [GNU Bash Manual - Conditional Constructs](https://www.gnu.org/software/bash/manual/bash.html#Conditional-Constructs)
- [Unix Test Operators - Tutorialspoint](https://www.tutorialspoint.com/unix/unix-basic-operators.htm)