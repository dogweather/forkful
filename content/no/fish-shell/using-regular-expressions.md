---
title:                "Fish Shell: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du ofte jobber med tekstbehandling og ønsker å effektivisere din arbeidsflyt, kan du dra nytte av å lære å bruke regex med Fish Shell. Regular expressions er en kraftig tekstbehandlingsverktøy som lar deg søke, erstatte og manipulere tekst basert på et mønster. Dette kan være nyttig når du for eksempel skal gjøre større endringer i en tekstfil eller når du skal finne og erstatte flere forekomster av et ord eller uttrykk.

## Hvordan

For å bruke regular expressions i Fish Shell, må du først åpne terminalen og navigere til mappen hvor filen du ønsker å arbeide med befinner seg. Deretter kan du bruke kommandoen ```cntl-v``` for å starte med å skrive mønsteret du ønsker å finne eller erstatte. For eksempel, hvis du ønsker å finne alle forekomster av ordet "fisk" i en tekst, kan du skrive mønsteret ```fisk``` og trykke ```enter```.

For å erstatte et mønster, kan du bruke kommandoen ```sed ```, som står for "stream editor". Dette er en vanlig kommando brukt i både Linux og Unix-systemer for å søke og erstatte tekst basert på et mønster. Med Fish Shell, kan du bruke denne kommandoen til å gjøre endringer i enkelte filer eller en hel mappe på en gang, avhengig av kommandoen du bruker.

Et annet nyttig verktøy for å arbeide med regex er ```grep```, som står for "global regular expression print". Denne kommandoen lar deg søke etter mønstre i en fil eller en hel mappe og kan også kombineres med andre kommandoer for å gjøre mer avanserte og detaljerte søk.

## Dypdykk

Når du begynner å mestre regex med Fish Shell, kan du også utforske mer avanserte funksjoner som fangstgrupper, kvantifisering og karakterklasser. Dette kan gi deg enda mer kontroll over teksten du arbeider med og hjelpe deg å finne og erstatte mønstre mer effektivt.

Det er også viktig å huske på at regex kan være forskjellig fra programmeringsspråk til programmeringsspråk. Så hvis du allerede har kjennskap til regex fra før, kan det være lurt å lese litt mer om hvordan det fungerer i Fish Shell spesifikt.

## Se også

- [Fish Shell sin offisielle nettside](https://fishshell.com/)
- [En omfattende tutorial for å lære mer om regex](https://regexone.com/)
- [Intro to Regular Expressions on Fish Shell]()
- [Bruke regex i tekstbehandling med Fish Shell](https://www.linux.com/training-tutorials/using-regular-expressions-text-processing-fish-shell/)