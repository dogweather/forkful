---
title:    "C++: Konvertere en streng til små bokstaver"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i programmering, spesielt når vi jobber med tekstbehandling, kan det være nødvendig å konvertere en streng til små bokstaver. Dette kan være for å sammenligne strenger, behandle data eller bare for å følge konvensjoner. Uansett årsak, å være i stand til å konvertere en streng til små bokstaver er et nyttig verktøy å ha i programmeringsarsenalet ditt. I denne bloggen vil vi utforske hvordan du kan gjøre dette med C++.

## Hvordan gjøre det

For å konvertere en streng til små bokstaver i C++, er det flere metoder du kan bruke. La oss se på noen eksempler:

```C++
#include<iostream>
#include<string>

int main() {
    // Opprett en streng
    std::string tekst = "HEST";

    // Bruk for-løkke for å iterere gjennom strengen
    for (int i = 0; i < tekst.length(); i++) {
        // Konverter hver bokstav til små bokstaver
        tekst[i] = tolower(tekst[i]);
    }

    // Skriv ut resultatet
    std::cout << tekst << std::endl;

    return 0;
}
```

Output:
```bash
hest
```

Her bruker vi for-løkke for å gå gjennom hver bokstav i strengen og bruke funksjonen `tolower` for å konvertere den til små bokstaver. Denne metoden er enkel og effektiv, men det er også andre metoder du kan bruke, for eksempel:

```C++
#include<iostream>
#include<string>
#include<algorithm>

int main() {
    // Opprett en streng
    std::string tekst = "HEST";

    // Bruk transform-funksjonen for å konvertere til små bokstaver
    transform(tekst.begin(), tekst.end(), tekst.begin(), ::tolower);

    // Skriv ut resultatet
    std::cout << tekst << std::endl;

    return 0;
}
```

Output:
```bash
hest
```

Her bruker vi `transform`-funksjonen og `::tolower` som argument for å konvertere hele strengen til små bokstaver. Dette er en kortere og mer effektiv måte å gjøre det på.

## Dykker dypere

Nå som vi har sett noen eksempler på hvordan vi kan konvertere en streng til små bokstaver, la oss se nærmere på noen viktige ting du bør huske på når du gjør dette:

- Hvis strengen inneholder spesialtegn eller bokstaver med aksent, kan de ikke konverteres til små bokstaver på denne måten. Du må i stedet bruke funksjoner som `tolower` fra `locale`-biblioteket for å håndtere slike bokstaver.
- Det finnes også andre biblioteker og metoder for å konvertere til små bokstaver, som `boost::to_lower` fra Boost-biblioteket. Det er verdt å utforske disse og finne den som fungerer best for ditt program.

## Se Også

- [C++ Strings](https://www.w3schools.com/cpp/cpp_strings.asp)
- [tolower in C++](https://www.geeksforgeeks.org/tolower-function-in-c/)
- [boost::to_lower](https://www.boost.org/doc/libs/1_72_0/doc/html/boost_algorithm/to_lower.html)