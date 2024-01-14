---
title:                "C++: Å finne lengden på en streng"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Å finne lengden på en streng er en essensiell del av å programmere i C++. Det lar deg håndtere tekstdata og utføre operasjoner på den. Det er også en grundig måte å forstå hvordan strenger fungerer i C++.

# Hvordan

For å finne lengden på en streng i C++, bruker vi funksjonen `strlen()`. La oss se på et eksempel:

```C++
#include <iostream>
#include <cstring>

int main() {
  char str[] = "Hei, verden!";
  std::cout << "Lengden på strengen er: " << strlen(str) << std::endl;
  return 0;
}
```

Output: `Lengden på strengen er: 13`

Vi inkluderer først `<cstring>` for å få tilgang til `strlen()`-funksjonen. Deretter deklarerer vi en char-array med navnet `str` og gir den en verdi på "Hei, verden!". Vi bruker så `strlen()`-funksjonen til å finne lengden på strengen og skriver den ut til konsollen.

Det er viktig å merke seg at `strlen()`-funksjonen returnerer et heltall, så hvis du vil lagre lengden på strengen i en variabel, må du bruke en `int`-variabel.

# Deep Dive

I C++, er strenger representert som en sekvens av tegn med en null-terminering (som er selve null-tegnet `'\0'` i spesifisert i ASCII-koden). I eksempelet ovenfor, har "Hei, verden!" en lengde på 13 tegn, med null-terminering inkludert.

I tillegg til `strlen()`-funksjonen, finnes det også en annen måte å finne lengden på en streng på i C++. Dette er ved å bruke `std::string`-klassen og dens `size()`-metode. La oss se på et eksempel:

```C++
#include <iostream>
#include <string>

int main() {
  std::string str = "Hei, verden!";
  std::cout << "Lengden på strengen er: " << str.size() << std::endl;
  return 0;
}
```

Output: `Lengden på strengen er: 13`

Som du kan se, kan vi også bruke `size()`-metoden på en `std::string`-variabel for å finne lengden på strengen.

# Se også

- [cppreference - strlen](https://en.cppreference.com/w/cpp/string/byte/strlen)
- [cppreference - std::string](https://en.cppreference.com/w/cpp/string/basic_string)
- [cplusplus.com - strlen](http://www.cplusplus.com/reference/cstring/strlen/)
- [cplusplus.com - std::string](http://www.cplusplus.com/reference/string/string/)