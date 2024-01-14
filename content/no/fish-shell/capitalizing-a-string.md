---
title:                "Fish Shell: Stor bokstav p å en streng"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor capitalisere en streng? Dette er et vanlig spørsmål man kan stille seg når man programmerer. Å capitalisere en streng betyr å gjøre den første bokstaven stor, mens resten av bokstavene forblir små. Dette kan være nyttig for å standardisere tekst eller for å lage en mer visuelt tiltalende utgang.

Det finnes flere måter å gjøre dette på, men i denne blogginnlegget vil vi spesielt se på hvordan man kan capitalisere en streng ved hjelp av Fish Shell. Fish Shell er et populært åpen kildekode-program som brukes som en erstatning for Bash Shell. Så la oss dykke inn i hvordan man capitaliserer en streng ved hjelp av Fish Shell.

# Hvordan Gjøre Det

La oss si at vi har en streng som heter "hallo verden" og vi ønsker å capitalisere den. Vi kan gjøre dette ved å bruke kommandoen "string capitalize" i Fish Shell:

```fish
string capitalize "hallo verden"
```

Output vil da bli "Hallo verden". Som du kan se, har den første bokstaven blitt gjort stor.

Men hva om vi ønsker å capitalisere en hel setning med flere ord? Dette kan også gjøres ved å bruke "string capitalize" kommandoen, men med et lite triks. Vi må nemlig bruke mellomrom som skilletegn mellom hvert ord:

```fish
string capitalize "dette er en setning"
```

Output vil da bli "Dette Er En Setning". Nå er hver første bokstav i hvert ord blitt gjort stor. Dette kan være nyttig for å lage overskrifter eller tittler i en tekst.

# Dypdykk

Hvorfor fungerer denne kommandoen slik? Ved å dykke dypere inn i Fish Shells dokumentasjon for "string capitalize" ser vi at den faktisk ikke bare gjør første bokstaven stor, men også bytter ut alle tegn som er ugyldige i et ord med mellomrom. Deretter capitaliserer den hver første bokstav i hvert ord og konverterer resten av bokstavene til små.

Det finnes også andre måter å capitalisere en streng på ved hjelp av Fish Shell, som for eksempel å bruke regex kommandoer. Men "string capitalize" kommandoen er den mest enkle og effektive måten.

# Se Også

- [Fish Shell dokumentasjon for string kommandoer](https://fishshell.com/docs/current/cmds/string.html)
- [GitHub for Fish Shell](https://github.com/fish-shell/fish-shell)
- [Enkel Guide til Fish Shell (på norsk)](https://digi.no/artikler/enkel-guide-fish-shell-pa-norsk/382366)