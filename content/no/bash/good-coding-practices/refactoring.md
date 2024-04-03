---
date: 2024-01-26 01:16:34.015951-07:00
description: "Omstrukturering er prosessen med \xE5 restrukturere eksisterende datamaskinkode\
  \ uten \xE5 endre dens eksterne oppf\xF8rsel. Det er en vital praksis for \xE5 redusere\u2026"
lastmod: '2024-03-13T22:44:40.983155-06:00'
model: gpt-4-0125-preview
summary: "Omstrukturering er prosessen med \xE5 restrukturere eksisterende datamaskinkode\
  \ uten \xE5 endre dens eksterne oppf\xF8rsel."
title: Refaktorering
weight: 19
---

## Hva & Hvorfor?
Omstrukturering er prosessen med å restrukturere eksisterende datamaskinkode uten å endre dens eksterne oppførsel. Det er en vital praksis for å redusere kompleksitet, forbedre vedlikeholdbarhet, og holde kodebasen sunn og lettere å forstå for både nåværende og fremtidige utviklere.

## Hvordan gjøre det:
La oss vurdere et enkelt Bash-skript som trenger litt omstrukturering. Det er klumpete, med gjentatt kode og vanskelig å følge:

```Bash
#!/bin/bash
echo "Skriv inn et filnavn:"
read filename
if [ -f "$filename" ]; then
    echo "Filen eksisterer."
    count=$(grep -c "foo" "$filename")
    echo "Ordet foo vises $count ganger."
else
    echo "Filen eksisterer ikke."
fi
```

Omstrukturering for klarhet og gjenbrukbarhet kan innebære å introdusere funksjoner og håndtere feil mer nådig:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Skriv inn et filnavn:"
    read -r filename
    echo "Skriv inn ordet du vil søke etter:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "Ordet $word vises $count ganger."
    else
        echo "Filen eksisterer ikke." >&2
        exit 1
    fi
}

main "$@"
```

Den omstrukturerte versjonen bruker funksjoner for å forbedre lesbarhet og muliggjøre potensielt gjenbruk.

## Dypdykk:
Omstrukturering er ikke et konsept som opprinnelig kom med Bash eller til og med høynivå programmeringsspråk; det er like gammelt som programmering selv. Begrepet ble formalisert i boken "Refactoring: Improving the Design of Existing Code" av Martin Fowler i 1999, med hovedfokus på objektorienterte språk.

I konteksten av Bash-skripting betyr ofte omstrukturering å bryte ned lange skript til funksjoner, redusere gjentakelse med løkker eller betingede uttrykk, og unngå vanlige fallgruver som å ikke håndtere mellomrom i filnavn. Alternativer til Bash for skript som har blitt for komplekse inkluderer Python eller Perl, som tilbyr bedre datastrukturer og feilhåndtering for komplekse oppgaver.

Bash-spesifikk omstrukturering handler mer om å følge beste praksis, som å sitere variabler, bruke `[[ ]]` for tester over `[ ]`, og foretrekke `printf` over `echo` for robust utdata. Implementeringsdetaljer dreier ofte seg om å følge stilguider og bruke verktøy som `shellcheck` for statisk analyse for å fange opp vanlige feil.

## Se også:
- [Googles skall stilguide](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, et statisk analyseverktøy for skallskript](https://www.shellcheck.net/)
- [Kunsten ved kommandolinjen](https://github.com/jlevy/the-art-of-command-line)
