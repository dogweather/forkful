---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 
Å lese kommandolinje-argumenter innebærer å motta data direkte fra terminalen der programmet kjører. Programmerere gjør dette for å manipulere programmets oppførsel ved kjøring.

## Hvordan få det til:
La oss se på hvordan vi kan lese kommandolinje-argumenter i Fish Shell.

```Fish Shell
function greet
    echo "Hei $argv[1]"
end
```
Kjør denne funksjonen gir oss følgende:

```Fish Shell
> greet Verden
Hei Verden
```
Her benyttet $argv[1] for å representere første kommandolinje-argument.

## Dypdykk:
Fish Shell standardiserte det til $argv, en liste av argumenter. Dette skiller seg fra tidligere shells som brukte $1, $2, osv. for å representere argumenter enkeltvis. Det finnes alternativer til å lese kommandolinje-argumenter, som å bruke getopts for å lese flagger og parametre. Men for lesing av generelle argumenter, er $argv brukervennlig og enkel å forstå.

## Se Også:
For mer informasjon om $argv, se Fish Shell dokumentasjonen: [Fish $argv](https://fishshell.com/docs/current/cmds/set.html). For historisk sammenligning og mer detaljer om alternativer, se [Command-line argument (Wikipedia)](https://en.wikipedia.org/wiki/Command-line_argument).