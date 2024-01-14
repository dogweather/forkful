---
title:                "C++: Sletting av tegn som matcher et mønster"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange situasjoner i programmering hvor det kan være nødvendig å slette karakterer i en tekststreng som matcher et bestemt mønster. Dette kan være for å filtrere ut uønsket informasjon, eller for å formatere tekst på en spesiell måte. I denne bloggposten skal vi se nærmere på hvordan man kan slette karakterer som matcher et bestemt mønster i C++.

## Slik gjør du det
For å slette karakterer som matcher et mønster i en tekststreng, kan man bruke C++ funksjonen `erase` sammen med funksjonen `find_first_of`. Dette lar oss søke gjennom tekststrengen og slette alle karakterer som matcher et gitt sett av karakterer.

```C++
// Først definerer vi en tekststreng og mønsteret vi ønsker å matche
std::string tekst = "Hei, dette er en tekststreng!";
std::string mønster = "aeiou"; // Denne gir oss et sett av vokaler

// Nå iterer vi gjennom tekststrengen og sjekker om hvert tegn matcher mønsteret vårt
for(size_t i = 0; i < tekst.length(); i++){
    if(mønster.find_first_of(tekst[i]) != std::string::npos){
        // Hvis tegnet matcher mønsteret, sletter vi det ved å bruke erase funksjonen
        tekst.erase(i, 1);
        i--; // Vi må også gå ett steg tilbake for å ikke hoppe over neste tegn
    }
}

// Til slutt skriver vi ut den endrede tekststrengen
std::cout << tekst << std::endl;
```

Kjøringen av denne koden vil gi følgende output:

```
H, dtt r n txtstrng!
```

Som du kan se, er alle vokalene blitt slettet fra tekststrengen.

## Dypdykk
I eksempelet ovenfor brukte vi `std::string` og `find_first_of` funksjonen fra C++ standard biblioteket. Men det finnes også andre måter å slette karakterer som matcher et mønster i C++. For eksempel kan man bruke regular expressions fra `std::regex` biblioteket, eller man kan implementere sin egen funksjon for å søke og slette karakterer.

Det er også viktig å huske at denne metoden vil slette alle forekomster av karakterene som matcher mønsteret, og ikke bare de første. Derfor bør man være forsiktig med hvilke mønstre man bruker for å unngå utilsiktede endringer i tekststrengen.

## Se også
- [C++ string::erase referanse](https://www.cplusplus.com/reference/string/string/erase/)
- [C++ string::find_first_of referanse](https://www.cplusplus.com/reference/string/string/find_first_of/)
- [C++ regular expressions tutorial (engelsk)](https://www.regular-expressions.info/cpp.html)
- [C++ regex referanse (engelsk)](https://www.cplusplus.com/reference/regex/)