---
title:                "C++: Søke og erstatte tekst"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave i programmering, og det er derfor viktig å ha en god forståelse av hvordan det fungerer. Ved å lære å søke og erstatte tekst kan du spare mye tid og unngå manuelt arbeid.

## Slik gjør du det

For å søke og erstatte tekst i C ++, kan du bruke funksjoner som `find`, `replace` og `substr`.

La oss si at vi har en streng `tekst` som inneholder følgende: "Jeg elsker å kode i C ++!". For å søke etter ordet "C ++" og erstatte den med "C ++14", kan du bruke følgende kode:

```
#include <iostream>
#include <string>

using namespace std;

int main() {

    string tekst = "Jeg elsker å kode i C ++!";
    string sok = "C ++";
    string erstatning = "C ++14";

    size_t pos = tekst.find(sok); // finner posisjonen til ordet som skal erstattes

    tekst.replace(pos, sok.length(), erstatning); // erstatter ordet

    cout << tekst << endl; // skriver ut den oppdaterte strengen
    // output: Jeg elsker å kode i C ++14!

    return 0;
}
```

Her bruker vi `find` for å finne posisjonen til ordet "C ++" i strengen `tekst`. Deretter bruker vi `replace` for å erstatte dette ordet med "C ++14". Etter at dette er gjort, skrives den oppdaterte strengen ut.

Du kan også bruke `substr`-funksjonen for å søke etter en del av en streng og erstatte den med en annen del. For eksempel, hvis vi vil erstatte "elsker" med "setter pris på", kan vi bruke følgende kode:

```
string erstatning = "setter pris på";
size_t pos = tekst.find("elsker");
tekst.replace(pos, 6, erstatning); // 6 er lengden til "elsker"
// output: Jeg setter pris på å kode i C ++14!
```

## Dykk dypere

For mer avanserte søk og erstatting, kan du bruke regulære uttrykk. Dette er spesielle mønstre som forteller programmet ditt hva du ønsker å søke etter og erstatte. For å bruke regulære uttrykk i C ++, må du inkludere `<regex>` -biblioteket.

En vanlig bruk av regulære uttrykk er å erstatte alle forekomster av et bestemt tegn eller uttrykk i en tekst. For eksempel, hvis vi vil erstatte alle mellomrom i en streng med understreker, kan vi bruke følgende kode:

```
#include <iostream>
#include <string>
#include <regex>

using namespace std;

int main() {

    string tekst = "Jeg elsker å kode i C++!";
    regex re(" "); // søk etter mellomrom

    cout << regex_replace(tekst, re, "_"); // erstatter med understrek
    // output: Jeg_elsker_å_kode_i_C++!

    return 0;
}
```

I dette tilfellet bruker vi `regex_replace`-funksjonen til å erstatte alle forekomster av mellomrom med en understrek. En annen vanlig bruk av regulære uttrykk er å søke etter visse mønstre og erstatte dem med andre.

## Se også

- [C ++ String manipulation](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [Regular expressions in C ++](https://www.w3schools.com/cpp/cpp_regex.asp)