---
title:    "C++: Store bokstaver i en streng"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne kapitalisere en streng (eller en rekke tegn) er en viktig ferdighet når du jobber med programmeringsspråk som C++. Å kapitalisere en streng betyr å gjøre alle bokstaver i strengen store. Dette kan være nyttig for å formatere utskrifter eller for å sammenligne tekster.

## Slik gjør du det

Kapitalisering av en streng kan oppnås på forskjellige måter, avhengig av hvilket programmeringsspråk du jobber med. I C++, kan du bruke standardfunksjoner for å kapitalisere en streng, som `toupper()` eller `toupper_l()`. Disse funksjonene vil konvertere alle små bokstaver i en streng til store bokstaver.

Her er et eksempel på hvordan du kan kapitalisere en streng i C++:

```C++
#include <iostream>
#include <string>
#include <locale>

int main()
{
    std::string streng = "dette er en test";
    
    // Bruker toupper_l() for å kapitalisere strengen
    std::locale loc;
    for (std::string::size_type i = 0; i < streng.length(); ++i)
        streng[i] = std::toupper(streng[i], loc);
        
    std::cout << streng << std::endl; // Output: "DETTE ER EN TEST"
    
    return 0;
}
```

Som du kan se, brukte vi `toupper()`-funksjonen sammen med `locale`-klassen for å kapitalisere strengen vår. Dette vil fungere for alle typer strenger, uavhengig av lengde eller om den inneholder tall eller symboler.

## Dypdykk

For å forstå hvordan funksjonene `toupper()` og `toupper_l()` fungerer, må du forstå konseptet med *tegnsett*. Et tegnsett er en liste over tegn og deres tilhørende numeriske verdier, og det brukes til å representere tekst i et dataprogram.

I C++, brukes tegnsettet Unicode for å representere tekst. Dette tegnsettet har over 137 000 unike tegn, og hver har sin egen numeriske verdi. Når du bruker `toupper()`-funksjonen, sammenligner den tegnene i strengen din med en liste over store bokstaver i Unicode-tegnsettet og erstatter dem deretter med den tilsvarende store bokstaven.

## Se også

- [Cppreference.com - toupper()](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [Cppreference.com - toupper_l()](https://en.cppreference.com/w/cpp/locale/toupper)
- [Unicode Character Table](https://unicode-table.com/en/)