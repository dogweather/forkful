---
title:                "Bash: Ekstrahering av substringer"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang lurt på hvordan du kan trekke ut en del av en tekststreng i et Bash-skript? Kanskje du har en fil med mange adresser og trenger bare å få ut postnummeret? Å trekke ut substrings er en nyttig og effektiv måte å behandle tekst på, og det kan spare deg mye tid og arbeid når du jobber med store datamengder.

## Slik gjør du det
For å trekke ut en del av en tekststreng i Bash, bruker du kommandoen `cut`. Denne kommandoen lar deg angi start- og sluttposisjonen for den delen av strengen du vil få ut. La oss si at du har en fil med massevis av e-postadresser, og du bare vil få ut brukernavnene. Du kan bruke følgende kommando:

```Bash
cut -d "@" -f 1 email_list.txt
```
Dette kommandoen deler hver linje i filen ved hjelp av "@" som skiller, og tar den første delen som brukernavn. La oss si at du har en fil som inneholder følgende:

```Bash
john.smith@example.com
jane.doe@example.com
```
Etter å ha kjørt kommandoen, vil du få følgende output:

```Bash
john.smith
jane.doe
```
Du kan også bruke `cut` til å få ut deler av strenger basert på et bestemt antall tegn, i stedet for posisjon. For eksempel, la oss si at du har en tekststreng som består av et navn etterfulgt av et ID-nummer, og du vil få ut bare navnet. Du kan bruke følgende kommando:

```Bash
cut -c 1-5 name_id.txt
```
Dette vil returnere de første 5 tegnene i hver linje i filen. Hvis fila ser slik ut:

```Bash
John123
Jane456
```

Vil output være:

```Bash
John1
Jane4
```

## Dykk dypere
Nå som du vet hvordan du skal bruke `cut` for å trekke ut substrings, kan du også utforske flere funksjoner og alternativer til denne kommandoen. For eksempel kan du bruke `-s` alternativet for å hoppe over linjer som ikke inneholder et bestemt tegn eller tegnsekvens. Du kan også bruke `-n` for å spesifisere et område av tegn som skal returneres uten noen ganger å bruke en separat fil for å definere det området.

Du kan også bruke kommandoen `grep` for å trekke ut substrings basert på et søkeord eller et mønster i teksten. Dette kan være nyttig for å filtrere ut bestemte data fra en større tekstfil.

## Se også
- [Offisiell dokumentasjon for cut command](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html)
- [W3Schools tutorial for Bash strings](https://www.w3schools.com/bash/bash_substrings.asp) 
- [Guide til grep command](https://www.geeksforgeeks.org/grep-command-in-unixlinux/)