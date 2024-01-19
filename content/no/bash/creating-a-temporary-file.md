---
title:                "Opprette en midlertidig fil"
html_title:           "Bash: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lage en midlertidig fil innebærer å opprette en fil som skal brukes en kort periode, for så å slettes. Programmerere gjør dette for å lagre data midlertidig uten å tynge hovedlagringen.

## Hvordan gjør man det:

Her er et enkelt eksempel på hvordan du lager og bruker en midlertidig fil i Bash:

```Bash
#!/bin/bash

# Opprett en midlertidig fil
tmpfile=$(mktemp)

# Skriv noe til filen
echo "Hei, verden!" > $tmpfile

# Vis innholdet i filen
cat $tmpfile

# Slett filen
rm $tmpfile
```

Når dette skriptet kjøres, vil det opprette en midlertidig fil, skrive "Hei, verden!" til den, vise innholdet, og slette filen. 

## Dypdykk

Lager vi midlertidige filer er en teknikk som går tilbake til de eldste dagene av programmering. Det brukes ofte i scenarioer hvor data må bufferes, eller hvor man jobber med store datasett.

Alternativene til midlertidige filer inkluderer bruk av databaser eller in-memory caching systemer som Redis. Men i mange tilfeller er midlertidige filer en enklere løsning.

Implementering av midlertidige filer i Bash er greit nok, men det er viktig å huske på å alltid slette filene når de ikke lenger er nødvendige. Hvis ikke, kan de ødelegge lagringsplassen over tid.

## Se også

For mer informasjon om midlertidige filer i Bash, se følgende kilder:

- Bash man-siden på midlertidige filer: `man mktemp` 
- [GNU Coreutils: mktemp](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html) 
- [Create and use temporary files in Bash](https://tldp.org/LDP/abs/html/tempfiles.html)