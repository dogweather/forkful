---
title:                "Store bokstaver i en tekststreng"
html_title:           "C++: Store bokstaver i en tekststreng"
simple_title:         "Store bokstaver i en tekststreng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være flere grunner til å kapitalisere en streng i et program. En vanlig grunn er å gjøre utdataen mer lesbar for brukeren. Det kan også være nødvendig for å følge konvensjoner og standarder i koding.

## Slik gjør du det

For å kapitalisere en streng i C++, trenger du å bruke en innebygd funksjon kalt `toupper()`. Denne funksjonen konverterer et enkelttegn til dets tilsvarende store bokstav. Ved å bruke en løkke, kan du gå gjennom alle tegnene i strengen og konvertere dem til store bokstaver.

```C++
// Eksempelkode for å kapitalisere en streng i C++
// Bruker en løkke og toupper() funksjonen

#include <iostream>
#include <cstring> // for å bruke strcpy() funksjonen
#include <cctype> // for å bruke toupper() funksjonen
using namespace std;

int main()
{
    char input[100]; // lagrer den opprinnelige strengen
    char output[100]; // lagrer den kapitaliserte strengen
    int len; // lengden på strengen

    // les inn den opprinnelige strengen fra brukeren
    cout << "Skriv inn en streng: ";
    cin.getline(input, 100);

    len = strlen(input);

    // bruk toupper() funksjonen til å konvertere hvert tegn
    // og lagre det i output strengen
    for (int i = 0; i < len; i++)
    {
        output[i] = toupper(input[i]);
    }

    // legg til en avsluttende null-tegn i output strengen
    output[len] = '\0';

    // skriv ut den kapitaliserte strengen
    cout << "Den kapitaliserte strengen er: " << output;

    return 0;
}

```

**Eksempel input:** Hei på deg!

**Eksempel output:** HEI PÅ DEG!

## Dypdykk

Mens `toupper()` funksjonen er den vanligste måten å kapitalisere en streng på i C++, er det flere andre metoder tilgjengelig. For eksempel kan du bruke `str.capitalize()` funksjonen fra `cstring` biblioteket i stedet for å lage en løkke. Denne funksjonen vil også konvertere første bokstav i hvert ord til stor bokstav, i tillegg til å konvertere enkelttegn.

Det finnes også tilgjengelige biblioteker og tredjepartsverktøy som tilbyr mer avanserte funksjoner for å kapitalisere strenger, som for eksempel å ignorere akronymer eller bestemte ord.

## Se også

- [C++ toupper() funksjon](https://www.programiz.com/cpp-programming/library-function/cctype/toupper)
- [C++ str.capitalize() funksjon](https://www.programiz.com/cpp-programming/library-function/cstring/strcapitalize)