---
title:    "C++: Å finne lengden av en streng"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden til en streng er en viktig oppgave når du jobber med tekstbehandling og manipulasjon av data i C++. Ved å beregne lengden av en streng, kan du enkelt utføre forskjellige operasjoner, som å sammenslå strenger, finne bestemte tegn og mye mer. Det er derfor viktig å forstå hvordan man kan finne lengden til en streng i C++.

## Hvordan

For å finne lengden til en streng i C++, kan du bruke funksjonen `strlen()` som finnes i `<cstring>`-biblioteket. Denne funksjonen tar inn en streng og returnerer antall tegn i strengen.

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    // Definer en streng og beregn lengden
    char streng[] = "Hei verden!";
    int lengde = strlen(streng);

    // Skriv ut resultatet
    cout << "Lengden til strengen er: " << lengde << endl;

    return 0;
}
```

Output:
```
Lengden til strengen er: 12
```

I tillegg til `strlen()` kan du også bruke funksjonen `.length()` på objekter av typen `string` for å finne lengden til en streng.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Definer en streng og beregn lengden
    string streng = "Hei verden!";
    int lengde = streng.length();

    // Skriv ut resultatet
    cout << "Lengden til strengen er: " << lengde << endl;

    return 0;
}
```

Output:
```
Lengden til strengen er: 12
```

## Dykk dypere

For å forstå hvordan `strlen()` fungerer, er det viktig å vite at C++ lagrer strenger som en sekvens av tegn, der hvert tegn er enkeltlagret i minnet. Når `strlen()` funksjonen går gjennom strengen, teller den antall tegn frem til den når et null-tegn (ASCII-verdi 0) som markerer slutten av strengen. Dette vil da være lengden til strengen.

Det er viktig å merke seg at `strlen()` ikke vil telle med det null-tegnet i lengden. Dette kan føre til feil hvis du ønsker å manipulere strengen på en måte som krever at null-tegnet blir inkludert. I slike tilfeller bør du heller bruke `.length()` funksjonen på en `string`-streng.

## Se også

- [C++ programvareutvikling hos Udacity](https://www.udacity.com/course/c-plus-plus-nanodegree--nd213)
- [String manipulation i C++ på GeeksforGeeks](https://www.geeksforgeeks.org/string-manipulation-in-c/)
- [C++ referansesider på cppreference.com](https://en.cppreference.com/w/)