---
title:                "C++: Stor bokstavløsning for en streng"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nødvendig å kapitalisere en streng i programmene våre. Dette kan være for å gjøre tekst mer leselig eller for å følge et spesifikt format. Uansett grunn, så kan programmeringsspråket C++ hjelpe oss med å gjøre dette enkelt.

## Hvordan

For å kapitalisere en streng i C++, kan vi bruke funksjonen `toupper()` fra `<cctype>` biblioteket. Her er et eksempel på hvordan vi kan gjøre dette:

```
#include <iostream>
#include <cctype>

using namespace std;

int main() {
    string tekst = "hallo verden";

    for (int i = 0; i < tekst.length(); i++) {
        tekst[i] = toupper(tekst[i]);
    }

    cout << tekst << endl;

    return 0;
}
```

Dette vil gi oss følgende utdata:

```
HALLO VERDEN
```

I dette eksempelet bruker vi en for-løkke for å iterere gjennom hver bokstav i strengen og konvertere den til store bokstaver ved hjelp av `toupper()` funksjonen. Deretter skriver vi ut den endrede strengen ved hjelp av `cout`.

## Dykk ned

I C++ er også `tolower()` funksjonen tilgjengelig fra `<cctype>` biblioteket, som kan brukes for å konvertere en streng til små bokstaver. Det finnes også andre teknikker for å kapitalisere en streng, som å bruke `std::transform()` funksjonen. Det er viktig å merke seg at disse funksjonene bare vil fungere for ASCII-tegn og ikke for alle typer tegnsett.

## Se også

- [C++ referanse for toupper() funksjonen](https://www.cplusplus.com/reference/cctype/toupper/)
- [C++ referanse for tolower() funksjonen](https://www.cplusplus.com/reference/cctype/tolower/)