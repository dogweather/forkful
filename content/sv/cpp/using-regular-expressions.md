---
title:                "C++: Att använda reguljära uttryck"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Varför använda reguljära uttryck i C++?

Om du är en erfaren C++ programmerare, har du förmodligen redan stött på terminologin "reguljära uttryck". Men vad är det egentligen och varför bör du använda det i dina program? Läs vidare för att ta reda på svaret!

## Så här använder du reguljära uttryck i C++

Reguljära uttryck är ett kraftfullt verktyg för strängmanipulering i C++. Det är en sekvens av tecken som definierar ett mönster för sökning och utbyte i en sträng. För att använda reguljära uttryck i C++, behöver du inkludera biblioteket *<regex>* och använda dess funktioner och klasser.

Här är ett exempel på hur du kan använda reguljära uttryck för att hitta alla ord i en sträng som börjar med bokstaven "s":

```C++
#include <iostream>
#include <regex>

int main() {
    std::string text = "Den snabba räven hoppade över den lata hunden.";
    std::regex pattern("s\\w+"); // definiera reguljärt uttryck
    std::smatch result; // för att lagra resultaten av sökningen

    // sök efter matchningar i strängen
    while (std::regex_search(text, result, pattern)) {
        // skriv ut matchningen
        std::cout << result.str() << std::endl;
        // "snabba", "snabba", "skutta", "snabba"

        // förflytta sökpositionen för nästa matchning
        text = result.suffix().str();
        // "räven hoppade över den lata hunden."
    }

    return 0;
}
```

Som du kan se söker reguljära uttryck efter ord som börjar med bokstaven "s" följt av ett eller flera bokstäver och skriver sedan ut matchningarna.

Det finns också andra användbara funktioner och klasser för reguljära uttryck i C++, som till exempel *regex_replace()* som ersätter matchningar i en sträng och *regex_iterator()* som hittar alla matchningar och låter dig utföra en handling på varje matchning.

## Djupdyka i användningen av reguljära uttryck

Vid första anblicken kan reguljära uttryck verka lite förvirrande och komplicerade, men det är värt att lära sig eftersom de kan göra strängmanipulering mycket mer effektivt. För att lära dig mer om reguljära uttryck och dess användningsområden i C++, kan du utforska dessa resurser:

- [C++ regex tutorial](https://www.learncpp.com/cpp-tutorial/standard-library/regular-expressions/): En utförlig handledning om reguljära uttryck i C++.
- [C++ reference: <regex> library](https://en.cppreference.com/w/cpp/regex): En omfattande referens för <regex>-biblioteket med detaljerade beskrivningar av funktioner och klasser.
- [Regex Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/): En användbar lista över reguljära uttrycksmönster och vad de betyder.

## Se även

Här är några andra användbara artiklar om C++ programmering som kan intressera dig:

- [C++ STL Containers för nybörjare](https://itteteam.blogspot.com/2019/04/c-stl-containers-for-beginners.html): En introduktion till C++ Standard Template Library Containers.
- [Felhantering i C++: Ett nybörjarguide](https://itteteam.blogspot.com/2019/07/error-handling-in-c-beginners-guide.html): Lär dig grunderna i felhantering i C++.
- [Arv och polymorfism i C++](https://itteteam.blogspot.com/2019/09/inheritance-and-polymorphism-in-c.html): En guide till arv och polymorfism i C++.

Jag hoppas att denna artikel varit till hjä