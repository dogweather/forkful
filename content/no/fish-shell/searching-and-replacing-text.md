---
title:                "Søking og erstattning av tekst"
html_title:           "Fish Shell: Søking og erstattning av tekst"
simple_title:         "Søking og erstattning av tekst"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Lurer du på hvordan du kan effektivt endre tekst i terminalen din? Fish Shell-klargjøringsprogrammet gir deg muligheten til å søke og erstatte tekst på en enkel og effektiv måte. Her vil du lære hvordan du kan gjøre dette ved hjelp av noen enkle kommandoer.

## Slik gjør du det

Søke og erstatte tekst ved hjelp av Fish Shell er enkelt og kan gjøres med bare noen få kommandoer. Følg disse trinnene for å lære hvordan:

1. Åpne terminalen din og skriv inn ```fish``` for å aktivere Fish Shell.
2. Bruk kommandoen ```cd``` for å navigere til mappen hvor filen du vil gjøre endringer i befinner seg.
3. Skriv kommandoen ```sed -i 's/<gammel tekst>/<ny tekst>/g' <filnavn>``` og trykk enter. Dette vil søke etter all forekomst av den gamle teksten og erstatte den med den nye teksten i filen du har valgt.

Etter at kommandoen er utført, vil du se en bekreftelse på at endringene er gjort. For å se resultatet kan du åpne filen og se at den gamle teksten er erstattet med den nye. 

## Dypdykk

Fish Shell tillater også å bruke regulære uttrykk i søke- og erstatningskommandoen. Dette kan være nyttig hvis du ønsker å søke etter flere varianter av den gamle teksten og erstatte dem med forskjellige nye tekster.

Du kan også bruke valglinjen til å spesifisere hvilke deler av teksten som skal endres. For eksempel ```s/<gammel tekst>/<ny tekst>/2``` vil bare endre den andre forekomsten av den gamle teksten, og la de andre være uendret.

Fish Shell har også en interaktiv søke- og erstatningskommando hvor du kan endre teksten etter behov, i stedet for å erstatte all tekst på en gang.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Søke og erstatte med sed](https://www.gnu.org/software/sed/manual/sed.html#Sed-Program-Overview)
- [Regulære uttrykk i grep og sed](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)