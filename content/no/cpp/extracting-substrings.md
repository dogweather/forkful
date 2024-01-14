---
title:                "C++: Utvinne undersstrenger"
simple_title:         "Utvinne undersstrenger"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor
I programmering er det ofte nødvendig å håndtere tekststrenger. Noen ganger kan det være behov for å hente ut en del av en streng, enten for å bruke den som et annet argument eller for å analysere den videre. Dette er hvor substring-ekstraksjon kommer inn i bildet. Ved å lære hvordan man ekstraherer substrings, kan man gjøre prosessen med å manipulere tekststrenger mer effektiv og nøyaktig.

# Hvordan du gjør det
Ekstrahering av substrings i C++ kan gjøres ved hjelp av funksjonen `substr()` i standardbiblioteket `string`. Denne funksjonen tar to argumenter: start-indeks og lengde på den ønskede substringen.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string tekst = "Hei dette er en tekst";
    // Ekstraherer substringen "er en"
    string subtekst = tekst.substr(11, 5);
    cout << subtekst << endl; // output: "er en"
    return 0;
}
```

Det er også mulig å ekstrahere substrings ved hjelp av indeksering av tegn i en string.

```C++
string tekst = "Hei dette er en tekst";
// Ekstraherer substringen "dette er"
string subtekst = tekst.substr(4, 9);
```

Det er viktig å merke seg at start-indeksen er null-indeksert, det vil si at første tegn i strengen er på indeks 0. Derfor vil for eksempel `tekst.substr(0, 3)` returnere substringen "Hei".

# Dypdykk
I C++ er strenger representert som en samling av tegn, eller en array. Derfor kan vi også bruke array-basert tilnærming for å ekstrahere substrings.

```C++
string tekst = "Hei dette er en tekst";
// Ekstraherer substringen "dette er"
string subtekst;
for (int i = 4; i < 13; i++) {
    subtekst += tekst[i];
}
```

En annen nyttig funksjon for substring-ekstraksjon er `find()` som finner første forekomst av en gitt streng eller tegn i en annen streng. Dette kan være nyttig når man ikke kjenner den nøyaktige start-indeksen til den ønskede substringen.

```C++
string tekst = "Hei dette er en tekst";
// Ekstraherer substringen "en tekst"
size_t pos = tekst.find("en tekst");
string subtekst = tekst.substr(pos, tekst.length() - pos);
```

# Se også
- [String funksjoner i C++](https://www.cplusplus.com/reference/string/string/)
- [Eksempler på substring-ekstraksjon i C++](https://www.programiz.com/cpp-programming/examples/substring)
- [Arrays i C++](https://www.cplusplus.com/doc/tutorial/arrays/)