---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Fish Shell: Søke og Erstatte Tekst 

## Hva & Hvorfor?

Søking og erstatting er prosessen med å finne spesifikk tekst (ord, setninger, tegn) og bytte det ut med noe annet. Programmerere gjør dette for å fjerne feil, forbedre kode, eller tilpasse data.

## Hvordan gjør man det:
Her er noen eksempler på how du kan søke og erstatte tekst i Fish Shell:

```Fish Shell
# Eksempel 1: Erstatte 'katt' med 'hund' i en tekstfil
string replace "katt" "hund" (cat dinfil.txt)

# Eksempel 2: Erstatte alle forekomster av 'katt' med 'hund' i en tekstfil
string replace -r "katt" "hund" (cat dinfil.txt)
```
Eksempel utsikt:
```Fish Shell
# Eksempel 1:
hund sitter på taket. 

# Eksempel 2:
hund ligger på sofaen. hund jakter på en mus.
```
## Dyp Dykk:

Historisk sett har søking og erstatting vært en sentral del av tekstbehandling, spesielt i programmering. Det gjør det mulig å gjøre store forandringer på data og kode effektivt.

Alternativer til Fish Shell inkluderer Bash, Zsh og PowerShell, hver med deres egen metode for søking og erstatting.

Implementasjonen av søking og erstatting i Fish Shell er basert på `string` funksjonen, som har innebygd støtte for søk og erstatte operasjoner.

## Se Også:

- Fish Shell Dokumentasjon: [string funksjon](https://fishshell.com/docs/current/cmds/string.html) 
- Stack Overflow: [Søke og erstatte i Fish Shell](https://stackoverflow.com/questions/tagged/fish)
- Fish Shell Github Repo: [Søking og erstatting eksempel koder](https://github.com/fish-shell/fish-shell)