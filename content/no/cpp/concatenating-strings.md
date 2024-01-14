---
title:                "C++: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor
Hvis du noen gang har jobbet med å behandle tekst i et C++ program, har du sannsynligvis kommet over situasjoner der du trenger å kombinere flere strenger til en enkelt streng. Dette kan være for å lage en komplett setning eller for å bygge en kompleks datastruktur. Uansett årsak, er konkatenering av strenger en viktig del av programmering og en ferdighet som absolutt er verdt å mestre.

# Hvordan konkatenering av strenger fungerer
For å begynne å konkatenere strenger i C++, trenger du først å inkludere standardbiblioteket <string>. Dette gir deg tilgang til funksjoner og objekter som er nødvendige for å jobbe med strenger. Deretter kan du bruke operatoren "+" for å kombinere to strenger. For eksempel:

```C++

#include <string>
#include <iostream>

int main() {
    string navn = "Per";
    string etternavn = "Hansen";

    string heltnavn = navn + etternavn;

    std::cout << heltnavn << std::endl;
}
```

Dette vil gi følgende utskrift:

```
PerHansen
```

Operatoren "+" kan også brukes til å kombinere flere strenger på en gang, for eksempel:

```C++
string ord1 = "Hei";
string ord2 = "alle";
string ord3 = "!";

string setning = ord1 + " " + ord2 + ", " + ord3;

std::cout << setning << std::endl;
```

Dette vil gi utskriften:

```
Hei alle, !
```

# Dypdykk i konkatenering av strenger
Det er viktig å merke seg at når du konkatenerer strenger i C++, opprettes det faktisk en helt ny streng som inneholder en kopi av de opprinnelige strengene. Dette betyr at hvis du jobber med store strenger eller konkatenerer dem flere ganger, kan dette føre til dårlig ytelse og bruk av minne.

For å unngå dette, kan du bruke klassen <string> sin funksjon "append()" istedenfor "+" operatoren. Denne funksjonen legger til en streng til slutten av en annen streng uten å opprette en ny streng. For eksempel:

```C++

string setning = "Dette er en test";
setning.append(" av konkatenering.");

std::cout << setning << std::endl;
```

Dette vil gi utskriften:

```
Dette er en test av konkatenering.
```

En annen viktig ting å merke seg er at konkatenering av strenger kan være en ressurskrevende prosess. Derfor er det lurt å unngå å gjøre det i store løkker, spesielt hvis du håndterer store strenger. Du kan også bruke funksjoner som "reserve()" for å forhåndsreservere plass i minnet for å redusere behovet for å øke størrelsen på strengen under konkateneringsprosessen.

# Se også
- [C++ String Concatenation Guide](https://www.guru99.com/cpp-string-class.html#10)
- [C++ String Concatenation vs Append](https://www.tutorialandexample.com/c-plus-plus-string-concatenation-vs-append/)
- [C++ String Class Reference](https://www.cplusplus.com/reference/string/string/)