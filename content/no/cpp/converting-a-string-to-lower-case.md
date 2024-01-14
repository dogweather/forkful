---
title:    "C++: Konvertere en streng til små bokstaver"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Konvertering av en streng til små bokstaver er en vanlig oppgave i programmering. Dette kan være nyttig når man for eksempel skal sammenligne strenger eller behandle brukerinput. Derfor er det viktig å ha en god forståelse for hvordan dette gjøres i C++.

## Hvordan gjøre det

For å konvertere en streng til små bokstaver i C++, kan du bruke den innebygde funksjonen "tolower" fra standardbiblioteket. Denne funksjonen vil konvertere alle store bokstaver i en streng til små bokstaver. Her er et eksempel på hvordan dette kan gjøres:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    string streng = "Hei på deg";
    for (int i = 0; i < streng.length(); i++) {
        // Bruk tolower-funksjonen for å konvertere hvert tegn
        streng[i] = tolower(streng[i]);
    }
    cout << streng << endl;
}

// Output: hei på deg
```

Her bruker vi en for-løkke for å gå gjennom hvert tegn i strengen og konvertere det ved hjelp av tolower-funksjonen. Det er viktig å merke seg at denne funksjonen kun fungerer for enkeltegnsstrenger, så hvis du ønsker å konvertere en hel setning, må du bruke en løkke som i eksempelet over.

## Dypdykk

Mens tolower-funksjonen er den enkleste måten å konvertere en streng til små bokstaver på, er det også andre metoder du kan bruke. For eksempel kan du bruke transform-funksjonen fra "algorithm" -biblioteket for å konvertere en hel streng på en enkel måte. Det vil se slik ut:

```C++
// Inkluder "algorithm" -biblioteket
#include <algorithm>

// Bruk transform-funksjonen
transform(streng.begin(), streng.end(), streng.begin(), ::tolower);
```

I tillegg er det viktig å være oppmerksom på at tolower-funksjonen kun fungerer på ASCII-tegn. Hvis du arbeider med tegn utenfor ASCII-tabellen, kan du bruke funksjoner som towlower eller wctrans for å konvertere til små bokstaver.

## Se også

- [C++ string class](https://www.cplusplus.com/reference/string/string/)
- [transform function](https://www.cplusplus.com/reference/algorithm/transform/)
- [tolower function](https://www.cplusplus.com/reference/cctype/tolower/)