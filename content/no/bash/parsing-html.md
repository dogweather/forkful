---
title:                "Bash: Parsing av HTML"
simple_title:         "Parsing av HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor du bør begynne å programmere i Bash for å analysere HTML

I dagens teknologidrevne verden er det en nødvendighet å kunne behandle og analysere data fra ulike kilder. Dette inkluderer også data fra nettsteder som er skrevet i HTML. Ved å lære å programmere i Bash og bruke riktige verktøy kan du enkelt trekke ut ønsket informasjon fra HTML-kode og behandle den på en effektiv måte.

## Slik gjør du det

Bash er et populært skriptspråk som er mye brukt i Linux-systemer, men det kan også brukes på MacOS og Windows. Det er gratis og enkelt å lære seg, så det er ingen grunn til å ikke komme i gang med å lære å bruke Bash til å analysere HTML.

Det første du trenger er et verktøy kalt "lynx", som kan hente ut innholdet fra en nettside og skrive det til en fil. Dette gjøres ved å bruke følgende kommando:

```Bash
lynx -dump http://www.example.com > output.html
```
Dette vil hente ut HTML-koden fra nettsiden og skrive det til en fil som heter "output.html". Deretter kan du bruke verktøyet "sed" til å fjerne alt som ikke er relevant for deg, som for eksempel reklame, bilder og annet distraksjonsinnhold. Dette gjøres ved å bruke følgende kommando:

```Bash
sed '/<img/d;/<script>/d;/<head>/,/<\/head>/d' output.html > clean.html
```

Denne kommandoen vil fjerne alle bilder, scripts og head-elementet fra filen "output.html" og skrive det til en ny fil som heter "clean.html". Nå kan du enkelt behandle den rensede HTML-koden videre ved hjelp av Bash-skript.

Et eksempel på et Bash-skript som henter ut alle lenker fra en nettside og skriver dem til en tekstfil, vil se slik ut:

```Bash
#!/bin/bash
lynx -dump http://www.example.com > output.html
sed '/<img/d;/<script>/d;/<head>/,/<\/head>/d' output.html > clean.html
grep -Eo "(http|https)://[a-zA-Z0-9./?=_-]*" clean.html > links.txt
```
Dette skriptet vil bruke de tidligere nevnte kommandoene og skrive alle lenker som finnes på en nettside til filen "links.txt". Det er kun fantasien som setter grensene for hvilken informasjon du kan trekke ut fra HTML ved hjelp av Bash-programmering.

## Dypdykk i HTML-analyse

Det finnes flere verktøy og kommandoer du kan bruke i Bash for å analysere HTML-kode. Noen nyttige kommandoer er "cut", "grep" og "awk", som kan brukes til å filtrere ut spesifikke deler av HTML-koden basert på søkeord eller mønstre. Du kan også bruke Bash til å utføre mer avanserte oppgaver som å ekstrahere tekst fra flere nettsider samtidig eller å lage automatiserte skript som oppdaterer filer basert på endringer i HTML-koden.

Det er viktig å være nøye med å lære deg riktig syntaks og format når du programmerer i Bash, da det kan være enkelt å gjøre feil som kan føre til uventede resultat. Heldigvis finnes det mange ressurser og forum på nettet hvor du kan få hjelp og råd hvis du står fast.

## Se også

- [Lær deg Bash-programmering med disse ressursene](https://www.linode.com/docs/tools-reference/how-to-write-bash-scripts/)
- [Bruk av grep-kommandoen i Bash](https://www.gnu.org/software/grep/manual/html_node/grep-Tutorial.html)
- [Innføring i sed-kommandoen](https://www.gnu.org/software/sed/manual/sed.html)