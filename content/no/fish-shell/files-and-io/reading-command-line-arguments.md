---
date: 2024-01-20 17:55:56.957456-07:00
description: "\xC5 lese kommandolinjeargumenter betyr \xE5 hente og bruke data som\
  \ brukeren skriver inn n\xE5r de kj\xF8rer et skript. Programmerere bruker dette\
  \ for \xE5 gj\xF8re skript\u2026"
lastmod: '2024-03-13T22:44:41.242874-06:00'
model: gpt-4-1106-preview
summary: "\xC5 lese kommandolinjeargumenter betyr \xE5 hente og bruke data som brukeren\
  \ skriver inn n\xE5r de kj\xF8rer et skript."
title: Lese kommandolinjeargumenter
weight: 23
---

## Hva & Hvorfor?
Å lese kommandolinjeargumenter betyr å hente og bruke data som brukeren skriver inn når de kjører et skript. Programmerere bruker dette for å gjøre skript fleksible og for å kunne kjøre diverse oppgaver basert på brukerinndata.

## Hvordan:
```Fish Shell
# eksempel_skript.fish
for arg in $argv
    echo "Argument: $arg"
end
```
Kjør skriptet: `fish eksempel_skript.fish ett to tre`
Forventet utskrift:
```
Argument: ett
Argument: to
Argument: tre
```

## Dypdykk:
Fish Shell, altså "Friendly Interactive SHell", er et moderne, brukervennlig skallet alternativ til andre shells som bash og zsh. Historisk sett var det mindre fokus på brukervennlighet, men Fish har endret dette med script som er lettere å forstå. Det støtter å lese kommandolinjeargumenter rett ut av esken, uten behov for komplekse syntakser. 

I motsetning til andre shells, der du kanskje må ty til `$@` eller `shift`, gir Fish deg `$argv`, som er en liste over argumentene. Det gjør looping gjennom argumentene intuitiv og enkel. Du kan til og med bruke indekser for å hente spesifikke argumenter, som `$argv[1]` for det første argumentet.

## Se Også:
- Fish Shell dokumentasjon for argumenter: https://fishshell.com/docs/current/#variables-command-line-arguments
- Tutorial for å skrive scripts i Fish: https://fishshell.com/docs/current/tutorial.html
- Stack Overflow for praksisspørsmål: https://stackoverflow.com/questions/tagged/fish
