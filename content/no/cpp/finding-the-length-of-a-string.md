---
title:                "Å finne lengden til en streng"
html_title:           "C++: Å finne lengden til en streng"
simple_title:         "Å finne lengden til en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng er en viktig grunnleggende ferdighet i programmering. Det lar oss manipulere og behandle tekster på en effektiv måte, noe som er essensielt i mange programmer.

## Slik gjør du det

For å finne lengden til en streng, kan du bruke funksjonen `length()` i C++. Denne funksjonen tar inn en streng som parameter og returnerer lengden av strengen som et heltall. La oss se på et eksempel:

```C++
#include <iostream>
using namespace std;

int main() {
    string navn = "Per";
    cout << "Lengden til strengen er: " << navn.length() << endl;

    return 0;
}
```

Etter å ha kjørt koden, vil du få følgende utskrift:

```
Lengden til strengen er: 3
```

Du kan også bruke en `for`-løkke til å gå gjennom hver karakter i strengen og telle dem manuelt. Her er en annen måte å finne lengden til en streng på:

```C++
#include <iostream>
using namespace std;

int main() {
    string navn = "Per";
    int lengde = 0;
    for (int i = 0; navn[i] != '\0'; i++) {
        lengde++;
    }

    cout << "Lengden til strengen er: " << lengde << endl;

    return 0;
}
```

I begge eksemplene ovenfor blir lengden av strengen "Per" ble regnet som 3.

## Dypdykk

I C++, blir lengden til en streng lagret som et heltall av typen `int`, siden det er det mest effektive formatet for å arbeide med tekst. Det viktigste å være oppmerksom på er at lengden på en streng inkluderer alle tegn, også mellomrom og spesialtegn.

En annen viktig ting å merke seg er at indeksene til en streng starter fra 0, så den første karakteren vil alltid ha en indeks på 0. Dette betyr at hvis en streng har en lengde på 3, vil indeksene være 0, 1, og 2.

## Se også

- [C++ Strings Tutorial](https://www.geeksforgeeks.org/cpp-strings/) (engelsk)
- [Grunnleggende om strenger i C++](https://www.w3schools.com/cpp/cpp_strings.asp) (engelsk)