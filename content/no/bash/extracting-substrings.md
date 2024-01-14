---
title:                "Bash: Utvinning av delstrenger"
simple_title:         "Utvinning av delstrenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å utvinne substrings kan være nyttig når du trenger å isolere og manipulere bestemte deler av en tekststreng. Dette kan være spesielt praktisk når du jobber med tekstbaserte datafiler eller i tilfeller der du trenger å utføre ulike handlinger basert på visse deler av en tekst.

## Hvordan

Det finnes flere måter å utvinne substrings på i Bash. En av de enkleste måtene er å bruke kommandoen "cut". La oss si at vi har en tekstfil med navnene på ulike personer, og vi bare ønsker å hente ut etternavnene. Vi kan gjøre dette ved å bruke følgende kommando:

```Bash
cut -d " " -f2 persons.txt
```

I dette eksempelet bruker vi "cut"-kommandoen til å dele opp tekstfilen ved hjelp av mellomrom som skilletegn ("-d" argumentet) og deretter selektere den andre samlingen av ord ("-f2" argumentet). Dette vil resultere i en utgang som bare inneholder etternavnene til personene i filen.

En annen metode for å utvinne substrings er å bruke en kombinasjon av ulike Bash-kommandoer som "grep" og "sed". La oss si at vi har en lang tekststreng og vi bare ønsker å hente ut den første bokstaven. Vi kan gjøre dette ved å bruke følgende kommandoer:

```Bash
echo "lang tekststreng" | grep -o "." | sed -n 1p
```

Her bruker vi "grep"-kommandoen til å finne alle tegn ("-o" argumentet) i teksten og deretter bruker "sed"-kommandoen til å bare returnere den første linjen ("-n 1p" argumentet).

## Dykk dypere

I tillegg til disse enkle eksemplene, er det også mange andre alternativer og muligheter når det kommer til å utvinne substrings i Bash. Du kan for eksempel bruke forskjellige regex uttrykk for mer avanserte søk eller bruke variabler for å lagre og manipulere utvinnet tekst.

Det er også viktig å merke seg at Bash fungerer ganske likt uavhengig av hvilket operativsystem du bruker det på. Dette gjør det enkelt å lære det en gang og deretter bruke det på tvers av forskjellige plattformer.

## Se også

- [Cut Command in Unix (With Examples)](https://www.geeksforgeeks.org/cut-command-in-unix-with-examples/)
- [Basic Sed Commands](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux)
- [Bash Regex Tutorial](https://www.digitalocean.com/community/tutorials/an-introduction-to-regular-expressions-in-bash)
- [Bash Variable Substitution](https://linuxize.com/post/bash-variable-substitution/)