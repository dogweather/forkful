---
title:                "Bash: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer er en viktig del av Bash-programmering. Dette kan være nyttig når du trenger å behandle eller manipulere data midlertidig i en script, og ikke ønsker å påvirke de permanente filene dine. Midlertidige filer er også nyttige når du trenger å lagre midlertidig informasjon som skal brukes senere i en prosess. 

## Hvordan du gjør det

Opprettelsen av en midlertidig fil i Bash er enkelt og kan gjøres ved hjelp av innebygde kommandoer som "mktemp" og "rm". For å opprette en ny midlertidig fil, kan du bruke følgende kommando:

```Bash
TEMP_FILE=$(mktemp)
```

Dette vil opprette en ny tom fil i det midlertidige mappen som vil bli tildelt til variabelen "TEMP_FILE". Du kan deretter legge til ønsket innhold i filen ved hjelp av tekstredigeringsverktøy som "nano" eller "vi". 

Når du er ferdig med å bruke den midlertidige filen, må den fjernes for å frigjøre systemets ressurser. Dette kan gjøres ved å bruke kommandoen "rm" som følger:

```Bash
rm "$TEMP_FILE"
```

Dette vil slette den midlertidige filen permanent fra systemet ditt.

## Dykk ned

Både "mktemp" og "rm" kommandoene har flere alternativer som kan brukes for å tilpasse opprettelsen og fjerningen av midlertidige filer. For eksempel kan du bruke "-p" alternativet med "mktemp" for å velge en spesifikk mappe for å opprette den midlertidige filen i. Du kan også bruke "-d" alternativet for å opprette en midlertidig mappe i stedet for en fil. 

Det er også viktig å merke seg at midlertidige filer generelt er sårbar for sikkerhetstrusler, spesielt hvis de blir brukt i åpne eller delt systemer. Det anbefales derfor å slette midlertidige filer så snart de ikke lenger er nødvendige for å hindre uautorisert tilgang til viktig informasjon. 

## Se også

- [Bash's "mktemp" kommando](https://www.computerhope.com/unix/mktemp.htm)
- [Bash's "rm" kommando](https://www.computerhope.com/unix/rm.htm)
- [Artikkel: "Sikkerhet rundt midlertidige filer"](https://www.howtogeek.com/169888/securely-deleting-temporary-files-in-bash-scripts/)