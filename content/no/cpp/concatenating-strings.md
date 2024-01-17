---
title:                "Sammenkobling av strenger"
html_title:           "C++: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenkobling av strenger er når man tar to separate tekststrenger og kombinerer dem til en enkelt streng. Dette er en vanlig operasjon i programmering og brukes til å lage mer komplekse tekststrenger. 

Programmerere gjør dette for å lage mer dynamiske og varierte utskrifter eller meldinger, som for eksempel å legge til brukerinput i en forhåndsdefinert tekststreng. Det gjør også koden mer strømlinjeformet og enklere å lese.

## Slik gjør du det:
```
#include <iostream>
#include <string>
using namespace std;

int main() {
  string streng1 = "Hei ";
  string streng2 = "verden!";
  string sammenkobling = streng1 + streng2; 
  cout << sammenkobling; // Utskrift: Hei verden!
  return 0;
}
```

I dette eksemplet bruker vi ```+``` operatøren for å sammenkoble to strenger. Dette fungerer også for å legge til flere strenger eller variabler.

## Dypdykk:
Sammenkobling av strenger har eksistert i programmering helt siden tidligere språk som COBOL og Fortran. På den tiden var det vanlig å bruke funksjoner som CONCAT for å kombinere strenger. I dag blir dette gjort mer effektivt ved hjelp av konkatinering operatøren som vist ovenfor.

Alternativet til å sammenkoble strenger er å bruke en datotype som heter strengbuffer. Dette er en type buffertank som inneholder en streng og kan utvides når nye strenger blir lagt til. Det kan være nyttig når man sammenkobler en stor mengde strenger.

Videre er det viktig å være klar over at sammenkobling av strenger kan bli en dyr operasjon når det gjøres mange ganger i en løkke eller med store tekststrenger. Dette kan føre til ytelsesproblemer, spesielt på eldre datamaskiner.

## Se også:
- [C++ Referanse for strenger](http://www.cplusplus.com/reference/string/)
- [Wikipedia side om tekstmanipulering i COBOL](https://en.wikipedia.org/wiki/COBOL_string_handling)