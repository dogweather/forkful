---
title:                "Sammenslåing av strenger"
date:                  2024-01-20T17:34:12.647902-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenslåing av strenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenkjede av strenger betyr å lime sammen to eller flere tekstbitene ende til ende. Programmerere gjør dette for å bygge opp komplekse strenger fra mindre biter, formatere utdata, eller konstruere kommandoer dynamisk.

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