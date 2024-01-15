---
title:                "Søking og erstattning av tekst"
html_title:           "C++: Søking og erstattning av tekst"
simple_title:         "Søking og erstattning av tekst"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å søke og erstatte tekst er en viktig del av programmering. Det lar deg automatisk endre tekst i en fil eller kode, noe som sparer deg for tid og unødvendig manuelt arbeid. 

## Slik gjør du det

For å kunne søke og erstatte tekst i C++, bruk følgende syntaks:

```C++
string nyTekst = gammelTekst.replace(0, 3, "ny tekst");
```

Dette eksempelet erstatter den gamle teksten fra posisjon 0 til 3 med den nye teksten "ny tekst".

For å endre tekst i hele filen, kan du bruke en løkke som itererer gjennom hele filen og bruker `replace()`-funksjonen på hver linje. Du kan også bruke `find()`-funksjonen til å finne spesifikke deler av teksten du vil erstatte.

Et annet nyttig verktøy for å erstatte tekst er `regex`-biblioteket i C++. Dette lar deg bruke regulære uttrykk for å søke og erstatte tekst som matcher et bestemt mønster.

## Deep Dive

Når du søker og erstatter tekst, er det viktig å huske på at det kan være flere forekomster av den teksten du vil endre. Ved å bruke `replace()`-funksjonen, vil alle forekomstene bli erstattet. Hvis du bare vil erstatte den første forekomsten, kan du bruke `replace_first()`-funksjonen i stedet.

En annen viktig ting å huske på er at `replace()`-funksjonen endrer den opprinnelige strengen, mens `replace_copy()`-funksjonen oppretter en kopi av den opprinnelige strengen med den nye teksten. Dette kan være nyttig hvis du vil bevare den opprinnelige teksten.

## Se også

- [C++ string-referanse](https://www.cplusplus.com/reference/string/string/replace/)
- [C++ regex-referanse](https://www.cplusplus.com/reference/regex/)
- [How to use regular expressions in C++](https://www.geeksforgeeks.org/regular-expressions-in-c-c/)