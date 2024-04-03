---
date: 2024-01-20 17:34:12.647902-07:00
description: "Sammenkjede av strenger betyr \xE5 lime sammen to eller flere tekstbitene\
  \ ende til ende. Programmerere gj\xF8r dette for \xE5 bygge opp komplekse strenger\
  \ fra\u2026"
lastmod: '2024-03-13T22:44:40.964182-06:00'
model: gpt-4-1106-preview
summary: "Sammenkjede av strenger betyr \xE5 lime sammen to eller flere tekstbitene\
  \ ende til ende."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Slik gjør du det:
```Bash
# Enkel sammenkobling
hilsen="Hei"
navn="Ola"
hilsen_navn="$hilsen, $navn!"
echo $hilsen_navn  # Ut: Hei, Ola!

# Med variabler og tekst
dato=`date +%Y-%m-%d`
log_filnavn="logg-$dato.txt"
echo $log_filnavn  # Ut: logg-2023-03-15.txt

# Bruk av klammer for tydelighet
bruker="bruker1"
path="/home/${bruker}/dokumenter"
echo $path         # Ut: /home/bruker1/dokumenter
```

## Dypdykk:
Tilbake på 70-tallet da Unix ble til, var bash' forgjenger, sh (Bourne shell), også utstyrt med evnen til å sammenkjede strenger. Siden da er string-sammenkjedning blitt grunnleggende i skripting.

Alternativt kan programmerere bruke `printf` for mer kontroll over formateringen, eller kanskje `awk` og `sed` for mer komplekse tekstbehandlingsoppgaver.

Når det gjelder implementeringsdetaljer, bash behandler variabler og tekst literalt når de blir ekspandert. Det betyr at variabler blir byttet ut med deres verdier, og siden det ikke finnes et eget operatør for sammenkjedning, blir tekster automatisk sammenkjedet når de står ved siden av hverandre.

## Se også:
- Bash manualen: [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- Advance Bash-Scripting Guide: [ABS Guide](https://tldp.org/LDP/abs/html/)
- En diskusjon om tekstmanipulasjon i bash: [Stack Overflow: String manipulation](https://stackoverflow.com/questions/tagged/string+bash)
