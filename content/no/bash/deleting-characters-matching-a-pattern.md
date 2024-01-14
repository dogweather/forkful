---
title:                "Bash: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være flere grunner til å slette tegn som matcher et visst mønster i Bash-programmering. Noen ganger kan det være nødvendig for å manipulere tekststrenger eller variabler, mens andre ganger kan det være en del av en større kodeprosess for å filtrere eller sortere data.

## Hvordan
Vi kan bruke Bash-kommandoen "sed" for å slette tegn som matcher et gitt mønster. La oss si at vi har en liste med telefonnumre i forskjellige formater, for eksempel "(123)456-7890" og "123-456-7890". Vi ønsker å fjerne parentesene og bindestreken og bare beholde tallene. Vi kan gjøre dette ved å kjøre følgende kommando:

```Bash
sed 's/[()-]//g' < phone_numbers.txt
```

Her bruker vi "sed" kommandoen med s-erstatningsfunksjonen, etterfulgt av mønsteret vi vil erstatte (parentes og bindestrek) og deretter erstatningen (tom streng). "g" -flagget er også lagt til for å slette alle forekomster av mønsteret i hver linje. Resultatet vil være en liste med bare tall i forskjellige formater.

## Dykk dypere
For mer avanserte manipulasjoner, kan vi også bruke regulære uttrykk i kombinasjon med "grep" kommandoen for å slette tegn som matcher et bestemt mønster. Dette åpner opp muligheten til å lage mer komplekse søkemønstre og gjøre mer presis tekstbehandling.

## Se også
- [Linuxize - Using sed to Delete Lines Matching a Specific Pattern](https://linuxize.com/post/using-sed-to-delete-lines-matching-a-specific-pattern/)
- [linux.die.net - sed command examples](https://linux.die.net/man/1/sed)