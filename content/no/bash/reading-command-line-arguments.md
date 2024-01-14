---
title:                "Bash: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du skriver Bash-programmer, er det viktig å kunne lese kommandolinjeargumenter. Dette gir deg muligheten til å lage mer allsidige og interaktive skript som kan tilpasses ulike behov. Ved å kunne lese kommandolinjeargumenter, kan brukere kjøre skriptene dine med spesifikke innstillinger eller påvirkning, og dermed gi en bedre brukeropplevelse.

## Hvordan

Å lese kommandolinjeargumenter i Bash er enkelt og krever bare noen få linjer med kode. Først må du huske å inkludere "$@" i starten av skriptet ditt. Dette vil fange alle kommandolinjeargumentene som brukes når skriptet blir kjørt. Deretter kan du bruke "shift" -kommandoen for å sikre at alle argumentene blir lest på riktig måte.

La oss nå se på et eksempel:

```Bash
#!/bin/bash
echo "Velkommen til mitt skript!"
echo "Første argument: $1"
echo "Andre argument: $2"
echo "Tredje argument: $3"
```

I dette eksempelet vil du se at vi bruker "$1", "$2" og "$3" for å skrive ut de tre første kommandolinjeargumentene. Du kan deretter kjøre skriptet med følgende kommandoer: "./mitt_skript.sh argument1 argument2 argument3". Resultatet vil være:

```Bash
Velkommen til mitt skript!
Første argument: argument1
Andre argument: argument2
Tredje argument: argument3
```

## Deep Dive

Det finnes ulike måter å håndtere kommandolinjeargumenter på i Bash, avhengig av kompleksiteten og behovene til skriptet ditt. En metode er å bruke en "case" -struktur for å håndtere ulike alternativer og argumenter. Du kan også bruke variabler og løkker for å håndtere flere argumenter.

En annen viktig ting å huske på når du leser kommandolinjeargumenter er å validere input. Dette er spesielt viktig når brukere kan legge inn egne argumenter, da det kan føre til uønskede resultater eller feil i skriptet ditt. Bruk gjerne "if" -strukturer og betingelser for å sjekke om argumentene er gyldige før du bruker dem i skriptet ditt.

## Se også

- [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Shell Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial)
- [Bash Scripting Cheat Sheet](https://devhints.io/bash)