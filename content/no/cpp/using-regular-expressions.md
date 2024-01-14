---
title:                "C++: Å bruke regulære uttrykk"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noensinne sittet fast i en situasjon hvor du trenger å søke etter et bestemt mønster i en tekst, men det føles umulig å finne det manuelt? Her kommer regular expressions til unnsetning! Regular expressions, også kjent som regex, er et kraftig verktøy som lar deg søke, filtrere og manipulere tekst basert på et gitt mønster. Med regular expressions kan du gjøre tekstbehandling og søk mye mer effektivt og presist.

## Slik gjør du det

For å bruke regular expressions i C++, må du inkludere <regex> -biblioteket. La oss si at du har en streng som inneholder en e-postadresse, og du vil sjekke om den er gyldig. Du kan gjøre dette ved å bruke en regex-uttrykk som matcher et vanlig e-postmønster:

```C++
#include <iostream>
#include <regex>

int main() {
    std::string email = "john@example.com";
    std::regex pattern("\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\b", std::regex_constants::icase);
    if (std::regex_match(email, pattern)) {
        std::cout << "Gyldig e-postadresse!" << std::endl;
    } else {
        std::cout << "Ugyldig e-postadresse!" << std::endl;
    }
    return 0;
}
```

I dette eksemplet brukes funksjonene ```std::regex_match()``` og ```std::regex_constants::icase``` for å sjekke om e-postadressen matcher det gitte mønsteret. Husk å bruke dobbel backslash (```\```) når du definerer mønsteret, da ```\``` også er et escape-tegn i C++.

## Dykke dypere

Regular expressions har et omfattende sett med regler og metoder som kan være overveldende i begynnelsen. Men med litt øvelse og tålmodighet kan du mestre dette kraftige verktøyet. Her er noen nyttige tips og triks for å hjelpe deg med å bli en regex-ekspert:

- Bruk regex tester for å prøve ut mønstre før du bruker dem i koden din.
- Lær de forskjellige metoder og egenskapene til regex-objekter, for eksempel ```std::regex_search()``` og ```std::regex_replace()```.
- Øv deg på å bruke regulære uttrykk på forskjellige tekstformater for å få en bedre forståelse av hvordan de fungerer.
- Bruk kommentarer og linjeskift i dine regex-uttrykk for å gjøre dem mer leselige.
- Søk etter tutorials og eksempler på nettet for å få mer innsikt i bruken av regular expressions.

## Se også

- [cppreference - regex](https://en.cppreference.com/w/cpp/regex)
- [RegExr - Regex tester](https://regexr.com/)
- [C++ Tutorials - Regular expressions](https://www.learncpp.com/cpp-tutorial/regular-expressions/)