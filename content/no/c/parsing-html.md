---
title:                "Analyse av HTML"
html_title:           "C: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/parsing-html.md"
---

{{< edit_this_page >}}

## Hvorfor
HTML er det mest brukte språket for å bygge nettsider, og det er viktig å kunne hente informasjon fra disse nettsidene. Ved å lære å parse, eller analysere, HTML med C, kan du få tilgang til specifikke elementer og data på en enkel og effektiv måte.

## Hvordan 
Å parse HTML med C kan virke komplisert i begynnelsen, men det er egentlig ganske enkelt. Følg disse trinnene for å få en grunnleggende forståelse for parsing:

- Først av alt, må du inkludere `stdio.h` og `stdlib.h` bibliotekene i koden din.
- Deretter kan du bruke funksjonen `fopen()` for å åpne en HTML-fil, og `fclose()` for å lukke filen når parsingen er ferdig.
- For å lese og samle informasjon fra HTML-filen, kan du bruke funksjonen `fgetc()` for å lese et tegn av gangen.
- Ved hjelp av kontrollstrukturer, som `if` og `while` kan du utføre handlinger basert på hva som leses fra filen.
- Til slutt, kan du bruke funksjonen `printf()` for å skrive ut den hentede informasjonen.

Her er et enkelt eksempel på å parse en HTML-fil som inneholder en liste med bøker:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    char c;

    fp = fopen("bøker.html", "r"); // Åpner filen for lesing
    if (fp == NULL) {
        printf("Feil ved å åpne filen!");
        exit(1);
    }

    while ((c = fgetc(fp)) != EOF) { // Leser et tegn av gangen
        if (c == '<') { // Starter på et HTML-element
            while ((c = fgetc(fp)) != '>') {} // Leser informasjonen til elementet er ferdig
            printf("\n"); // Skriver ut informasjonen på en ny linje
        }
        else if (c == '&') { // Starter på et escape-tegn, for eksempel "&amp;"
            while ((c = fgetc(fp)) != ';') {} // Leser til tegnet er ferdig
        }
        else { // Vanlig tekst
            printf("%c", c); // Skriver ut tegnet
        }
    }

    fclose(fp); // Lukker filen

    return 0;
}
```

Eksempel output:

```
Hermann Hesse
Harry Potter og De vises stein
Fyren mellom verdener
Alice i Eventyrland
```

## Deep Dive
Parsing av HTML kan være mer avansert enn dette, spesielt når det kommer til å håndtere feil og uventede situasjoner. Det kan også være nyttig å se nærmere på hvordan forskjellige elementer er strukturert i HTML, for eksempel å bruke DOM (Document Object Model) til å navigere i et HTML-dokument.

En annen viktig aspekt ved parsing av HTML er å ta hensyn til ulike tegnsett og å håndtere manglende tegn i filen. Dette kan føre til uønskede feil i koden, så det er viktig å ha en god forståelse av hvordan man håndterer disse situasjonene.

## Se også
- [Hvordan parse en minimarkør i C](https://www.journaldev.com/7463/how-to-parse-html-file-in-c)
- [HTML Tutorial for beginners](https://www.w3schools.com/html/)
- [Document Object Model (DOM)](https://www.w3schools.com/js/js_htmldom.asp)