---
title:                "C++: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Hvorfor

Regulære uttrykk er et viktig verktøy for å behandle og manipulere tekststrenger i C++ programmering. Det gir en enkel og effektiv måte å søke etter og matche bestemte mønstre i tekst, og kan være svært nyttig for å behandle store datasett eller utføre komplekse tekstbehandlingsoppgaver.

##Slik gjør du det

For å bruke regulære uttrykk i C++, må du inkludere <regex> -biblioteket i koden din. Deretter kan du definere et regulært uttrykk ved å bruke std::regex-klassen. Her er et eksempel på en funksjon som søker etter og teller antall forekomster av et bestemt ord i en tekststreng:

```C++
#include <iostream>
#include <regex>

using namespace std;

int finn_antall_forekomster(string tekst, string ord) {
    regex r(ord); //definerer et regulært uttrykk med det gitte ordet
    int antall = 0;
    sregex_iterator iter(tekst.begin(), tekst.end(), r); //lager en iterator som sjekker gjennom hele tekststrengen
    sregex_iterator end; //iterator som viser til slutten av tekststrengen
    while (iter != end) { //så lenge iterator ikke har nådd slutten
        antall++; //øker antall forekomster med 1
        iter++; //flytter iterator til neste matchende mønster
    }
    return antall;
}

int main() {
    string tekst = "Dette er en tekststreng som inneholder flere forekomster av ordet tekst";
    string ord = "tekst";
    int antall = finn_antall_forekomster(tekst, ord);
    cout << "Antall forekomster av ordet tekst: " << antall << endl;
    return 0;
}
```
Resultatet av dette eksempelet vil være: Antall forekomster av ordet tekst: 2

Det er også mulig å bruke regulære uttrykk til å erstatte deler av tekststrenger eller utføre mer avanserte operasjoner som å splitte en tekststreng i flere deler basert på visse mønstre. Det er mange ressurser tilgjengelig på nettet som kan hjelpe deg med å lære mer om hvordan du bruker regulære uttrykk i C++.

##Dypdykk

Å forstå hvordan regulære uttrykk fungerer og hvordan de blir behandlet av datamaskinen kan være nyttig for å skrive mer effektiv kode. Regulære uttrykk bruker et begrep som kalles "regex engine" for å søke gjennom tekststrenger og finne matcher basert på de gitte mønstrene. Det finnes ulike typer regex engines og de kan oppføre seg litt forskjellig avhengig av hvilken programmeringsspråk eller applikasjon de brukes i.

Det er også viktig å være forsiktig med å bruke regulære uttrykk i forbindelse med mer komplekse oppgaver. Det kan være fristende å bruke ett stort og komplisert regulært uttrykk for å løse alle problemer, men det kan ofte føre til uforutsette feil eller vanskeligheter med å vedlikeholde koden. Det er derfor viktig å finne en balanse mellom å bruke regulære uttrykk og andre metoder for tekstbehandling.

##Se også

- [C++ std::regex reference](https://en.cppreference.com/w/cpp/regex)
- [Codecademy C++ Regular Expressions course](https://www.codecademy.com/learn/learn-regular-expressions)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)