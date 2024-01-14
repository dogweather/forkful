---
title:    "Bash: Sletting av tegn som matcher et mønster"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å ønske å slette tegn som matcher et gitt mønster i Bash-programmering. Kanskje du har en fil med uønsket formatering som du ønsker å fjerne, eller kanskje du ønsker å lage et skript som kan hjelpe deg å rense data.

## Hvordan

For å slette tegn som matcher et mønster, kan du bruke kommandoen `sed`. Denne kommandoen lar deg endre tekst på en enkel måte ved å bruke regulære uttrykk.

```Bash 
sed '/mønster/d' filnavn
```
Denne koden vil slette alle linjer som inneholder mønsteret som er spesifisert. For eksempel, hvis du ønsker å slette alle linjer i en fil som inneholder ordet "test", kan du bruke følgende kommando: 

```Bash
sed '/test/d' filnavn
```

Du kan også endre kommandoen for å matche et mer spesifikt mønster. For eksempel kan du slette alle linjer som starter med et bestemt ord ved å bruke `^`-tegnet. 

```Bash
sed '/^ord/d' filnavn
```

Dette vil slette alle linjer som starter med det spesifiserte ordet. Du kan også bruke `$`-tegnet for å matche slutten av en linje, og dermed slette alle linjer som slutter på et bestemt ord. 

```Bash
sed '/ord$/d' filnavn
```

Det er også mulig å bruke flere mønstre samtidig ved å bruke `|`-tegnet. Dette vil slette alle linjer som inneholder både det første og det andre mønsteret. 

```Bash
sed '/mønster1|mønster2/d' filnavn
```

## Dypdykk

For å forstå hvordan `sed` fungerer, er det nyttig å vite litt om regulære uttrykk. Regulære uttrykk er et kraftig verktøy for å matche og manipulere tekst. De brukes i mange programmeringsspråk og kommandoer, inkludert Bash. 

I `sed` brukes regulære uttrykk i kombinasjon med kommandoen `s`, som står for "substitute". Denne kommandoen lar deg erstatte tekst eller slette tekst basert på mønstre. For eksempel, hvis du ønsker å erstatte et ord med et annet, kan du bruke følgende kommando: 

```Bash
sed 's/gammelt_ord/nytt_ord/g' filnavn
```

Dette vil erstatte alle forekomster av `gammelt_ord` med `nytt_ord` i hele filen. Du kan også kombinere dette med regulære uttrykk for å matche mer spesifikt. For eksempel kan du erstatte alle linjer som starter med et tall med et annet ord. 

```Bash
sed 's/^[0-9]/ord/g' filnavn
```

Dette vil erstatte alle tall som starter en linje med ordet `ord`. 

## Se også

For å lære mer om regulære uttrykk og `sed`-kommandoen, kan du se på følgende ressurser:

- [Unix 'sed' command](https://www.computerhope.com/unix/used.htm)

- [Regular Expressions - GNU Bash Manual](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html)

- [Få grep på grep – Regulære uttrykk i bash](https://uninett-norstore.github.io/2016/06/30/grep-regular-expression.html)