---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil er prosessen med å generere en ikke-varig fil for å lagre data midlertidig. Programmerere gjør dette for å håndtere store datamengder, unngå minneoverbelastning og forbedre programytelse.

## Slik Gjør Du:
I Fish Shell kan vi lage en midlertidig fil ved hjelp av `mktemp`-kommandoen. Her er et praktisk eksempel:

```Fish Shell
set tempfil (mktemp)
echo 'Hei Verden!' > $tempfil
cat $tempfil
```
Etter å ha kjørt koden over, vil du se "Hei Verden!" skrevet tilbake i terminalen.

## Dypdykk
Opprettelse av midlertidige filer har lenge vært en del av programmeringsspråkhistorie for å administrere begrensede ressurser effektivt. Alternativer til `mktemp` i Fish Shell kan være bruk av `/dev/shm` i Linux, som er et minnebasert filsystem.

Vær oppmerksom på at midlertidige filer fra `mktemp` i Fish Shell ikke automatisk blir slettet, og det er utviklerens ansvar å slette dem for å unngå opphopning av unødvendige filer.

## Se Også
For mer informasjon og avanserte brukstilfeller, kan du besøke følgende ressurser:
- Fish Shell offisielle dokumentasjon: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- En dyptgående tutorial om mktemp: [https://www.tutorialspoint.com/unix_commands/mktemp.htm](https://www.tutorialspoint.com/unix_commands/mktemp.htm)