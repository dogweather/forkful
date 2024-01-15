---
title:                "Konvertere en streng til små bokstaver"
html_title:           "C++: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en streng til små bokstaver er en vanlig oppgave i programmering, spesielt når det kommer til behandling av brukerinput. Ved å gjøre dette, kan vi sikre at inndataene er i en ensartet form, og dermed gjøre det enklere å sammenligne og manipulere dem.

## Hvordan
```C++
#include <iostream>
#include <string>
#include <algorithm> //for å bruke transform()

using namespace std;

int main() {
    //definere en streng
    string tekst = "HeLlO WoRlD";

    //bruke transform() funksjonen for å konvertere strengen til små bokstaver
    transform(tekst.begin(), tekst.end(), tekst.begin(), ::tolower);

    //skrive ut den konverterte strengen
    cout << tekst; //output: hello world

    return 0;
}
```

## Dypdykk
I C++ kan vi bruke transform() funksjonen fra `<algorithm>` biblioteket for å konvertere en streng til små bokstaver. Denne funksjonen tar inn tre argumenter: starten av strengen, slutten av strengen og en "functor" som spesifiserer konverteringsreglene. I eksempelet ovenfor bruker vi `::tolower` som en "functor", som betyr at transform() vil konvertere hvert tegn i strengen til sin equivalente små bokstav.

## Se også
- C++ string funksjoner: https://www.cplusplus.com/reference/string/string/functions/
- Transform() funksjonen i `<algorithm>` biblioteket: https://www.cplusplus.com/reference/algorithm/transform/