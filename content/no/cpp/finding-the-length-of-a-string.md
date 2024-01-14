---
title:                "C++: Å finne lengden til en streng"
simple_title:         "Å finne lengden til en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng er en grunnleggende operasjon i C++, og det er en viktig ferdighet for alle som ønsker å bli dyktige programmerere. Å kjenne lengden på en streng gjør det mulig for deg å behandle og manipulere tekstdokumenter, input fra brukere og mye mer.

## Hvordan

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Opprett en streng
    string navn = "Lars";

    // Finn lengden på strengen ved å bruke size() funksjonen
    int lengde = navn.size();

    // Skriv ut lengden på strengen
    cout << "Lengden på navnet ditt er: " << lengde << endl;

    return 0;
}
```

**Output:**
```
Lengden på navnet ditt er: 4
```

Her ser vi at vi kan finne lengden på en streng ved å bruke `size()` funksjonen. Vi kan også bruke `length()` funksjonen, da den gjør det samme som `size()`. Begge funksjonene returnerer en `int` som representerer lengden på strengen.

## Dykk dypere

Det er viktig å være klar over at funksjonene `size()` og `length()` ikke alltid vil gi samme resultat. Selv om de fleste programmerere bruker dem om hverandre, har de faktisk en liten forskjell.

`length()` funksjonen er definert i headerfilen `<string>` og brukes i hovedsak for basisstrenger, som `const char*` eller `char*` , mens `size()` funksjonen er definert i både headerfilene `<string>` og `<vector>` og er generelt brukt for STL klasser som `string` eller `vector`.

I tillegg kan vi også bruke `for`-løkker eller `while`-løkker til å finne lengden på en streng ved å inkrementere en teller hver gang vi går gjennom en bokstav i strengen. Men dette krever mer kode og er ikke like praktisk som å bruke de innebygde funksjonene.

## Se også

- [cppreference - std::string::size()](https://en.cppreference.com/w/cpp/string/basic_string/size)
- [GeeksforGeeks - Finding length of a string in C++](https://www.geeksforgeeks.org/finding-length-of-a-string-in-cpp/) 
- [Programiz - C++ String size() function](https://www.programiz.com/cpp-programming/library-function/cstring/size)