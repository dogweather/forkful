---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?
Å finne lengden på en streng betyr å beregne antall tegn det inneholder. Programmører gjør dette for å manipulere eller transformere data effektivt.

## Hvordan:
La oss se på et kort eksempel på hvordan du kan finne lengden på en streng ved hjelp av C++'s innebygde funksjon `size()` eller `length()`.

```C++
#include<iostream>
#include<string>
using namespace std;

int main(){
    string tekst = "Hei, Norge!";
    cout << "Tekstens lengde er: " << tekst.length() << endl;
    return 0;
}
```
Output:
```
Tekstens lengde er: 11
```

## Dyp dykk
Funksjonene `length()` og `size()` er historisk identiske og ble definert i C++98. Du kan bruke begge for å få strengens lengde.

Alternativt, du kan også bruke C-style `strlen()` funksjonen, men det er mer utsatt for feil fordi det krever en null-terminert `char` array.

I standard C++ bibliotek, funksjonen `length()` returnerer antall tegn i strengen ved å bruke en konstant tidskompleksitet, noe som betyr at uansett størrelsen på strengen, vil tiden det tar å beregne dens lengde være konstant.

## Se også
For mer avanserte metoder for strengmanipulasjon i C++, se [std::string referanse](http://www.cplusplus.com/reference/string/string/). For en dypere forståelse av hvordan strenger fungerer i C++, sjekk ut [C++ strenger](https://en.cppreference.com/w/cpp/string/basic_string).