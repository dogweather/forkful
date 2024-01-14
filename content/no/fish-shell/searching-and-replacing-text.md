---
title:    "Fish Shell: Søking og erstatning av tekst"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle du bruke tid og energi på å søke og erstatte tekst? Vel, det er flere grunner til dette. Kanskje du ønsker å endre en bestemt tekst i en rekke filer, eller kanskje du ønsker å endre en del av en tekst i en fil. Uansett hva grunnen er, kan tekst-søk og erstatning være en nyttig og effektiv måte å gjøre endringer i store mengder med tekst.

## Hvordan

Fish Shell tilbyr en effektiv måte å søke og erstatte tekst i filer ved hjelp av kommandoen `sed`. La oss si at du ønsker å endre alle forekomster av ordet "farge" i en fil til "fargevalg". Du kan enkelt gjøre dette ved å følge dette mønsteret: 

```Fish Shell
sed -i 's/farge/fargevalg/g' filnavn
```

La oss si at du ønsker å gjøre dette i flere filer i en mappe. Her er en annen enkel måte å gjøre det på: 

```Fish Shell
sed -i 's/farge/fargevalg/g' *.txt
```

Dette vil søke etter alle filer med `.txt`-utvidelsen i mappen og endre forekomster av "farge" til "fargevalg" i hver enkelt fil.

## Dykk dypere

Det er også flere måter å tilpasse dette kommandoen på, avhengig av dine spesifikke behov. Du kan for eksempel begrense søket til en bestemt posisjon i teksten ved hjelp av tallrepresentasjon. Du kan også bruke regulære uttrykk for å gjøre søket enda mer presist og fleksibelt. Utforsk gjerne de forskjellige mulighetene ved å lese mer om `sed`-kommandoen i Fish Shell dokumentasjonen.

## Se også

* [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
* [Tutorial: Fish Shell for beginners (Norwegian)](https://www.ealdwulf.com/fish-shell-for-beginners/)
* [Introduction to sed command in Linux/Unix](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)