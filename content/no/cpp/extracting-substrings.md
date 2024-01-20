---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å utvinne understrenger er prosessen med å hente en del av en streng basert på bestemte kriterier. Programmerere gjør dette for å manipulere data, søke i tekster og traversere datasett mer effektivt.

## Hvordan:

C++ har en innebygd metode, `substr()`, for å hente understrenger. Her er en forenklet måte å bruke den på:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hei, verden!";
    std::string understreng = str.substr(4, 6);
    std::cout << understreng;  
    return 0;
}
```

Output:

```bash
verden
```

I dette tilfellet henter `str.substr(4, 6)` seks tegn fra indeksen `4`. Dette vil gi oss understrengen "verden" fra "Hei, verden!".

## Dybdestudium:

Understrenger ble introdusert i tidlige programmeringsspråk for å lette strengmanipulasjon og datahåndtering. Det ble raskt en nødvendighet i tekstbehandling ved utføring av oppgaver som søk og erstatt.

Det er alternative måter å hente understrenger, som bruk av stl-funksjonen `std::string::find()` og `std::string::erase()`. Valget av metode avhenger ofte av problemstillingen og hvordan den kan løses mest effektivt.

Implementasjonsdetaljer om `substr()` kan variere mellom forskjellige versjoner av C++. Men generelt returnerer `substr()` en ny streng som starter med tegnet på indeksen gitt som første argument, og fortsetter opp til, men ikke inkludert, tegnet på indeksen som er summen av de to argumentene.

## Se Også:

1. Cplusplus - [std::string::substr()](http://www.cplusplus.com/reference/string/string/substr/)
2. Stack Overflow - [Extracting a Substring in C++](https://stackoverflow.com/questions/36655574/extracting-a-substring-in-c)
3. CPP reference - [std::string::find()](https://en.cppreference.com/w/cpp/string/basic_string/find)