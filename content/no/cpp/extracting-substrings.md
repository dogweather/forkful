---
title:                "C++: Uttrekk av delstrenger"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger i kodingen vår trenger vi bare en del av en streng. Dette kan være for å søke etter et bestemt ord eller uttrykk, eller for å behandle en del av en tekst på en annen måte enn resten. Å ekskludere substrings kan være en kraftig måte å manipulere data på i C++.

## Hvordan

For å ekstrahere en substring i C++, må vi bruke funksjonen `substr()`. Denne funksjonen tar to parametere: startposisjonen for substrings og lengden på substrings vi ønsker å ekskludere fra den opprinnelige strengen. La oss se på et eksempel:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string name = "Jonathan";
    string last_name = name.substr(3, 3);
    cout << last_name;

    return 0;
}
```

I dette tilfellet vil utgangen være "tha", siden vi starter på indeks 3 (som er bokstaven "t" i "Jonathan") og trekker ut tre tegn fra strengen.

Vi kan også bruke funksjonen `find()` for å finne posisjonen til et bestemt tegn eller uttrykk i en streng. Dette kan være nyttig hvis vi ikke vet den nøyaktige startposisjonen for substrings. La oss se på et annet eksempel:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string greeting = "God morgen, alle sammen!";
    int pos = greeting.find(",");
    string time = greeting.substr(pos + 2, 9);
    cout << "Klokken er " << time << " om morgenen.";

    return 0;
}
```

Her finner vi posisjonen til kommaet i strengen og bruker deretter `substr()` til å ekskludere tiden fra strengen.

## Dypdykk

Når vi bruker `substr()` i en løkke, kan det være lurt å være oppmerksom på at `substr()` endrer startposisjonen for strenger. Dette kan føre til uventede resultater hvis vi ikke tar hensyn til det. For å unngå dette, kan vi bruke `substr()` sammen med funksjonen `erase()` som sletter en del av en streng uten å endre startposisjonen.

## Se også

- [C++ string-funksjoner fra cplusplus.com](https://www.cplusplus.com/reference/string/string/)
- [Offisiell C++ dokumentasjon for string-funksjoner](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- [Et innblikk i C++ strenger fra learncpp.com](https://www.learncpp.com/cpp-tutorial/char-arrays-and-stdstrings/#substring)