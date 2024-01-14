---
title:                "C++: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver kan vara en viktig del av att hantera användarinput eller behandla data på ett enhetligt sätt. Detta kan vara speciellt användbart när man jämför strängar eller gör sökoperationer.

## Såhär gör man

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Skapa en sträng med blandade bokstäver
    string str = "HeLLo WoRLd";

    // Konvertera strängen till små bokstäver
    for (int i = 0; i < str.length(); i++) {
        str[i] = tolower(str[i]);
    }

    // Skriv ut konverterade strängen
    cout << str << endl; // hello world

    return 0;
}
```

## Djupdykning

I C++ finns det flera sätt att konvertera en sträng till små bokstäver. En metod är att använda en loop och `tolower()` funktionen för att ändra varje enskild bokstav. En annan metod är att använda `transform()` funktionen tillsammans med `tolower()` för att konvertera hela strängen på en gång.

Det finns också bibliotek och externa funktioner som kan användas för att konvertera en sträng till små bokstäver, som `boost::algorithm::to_lower()` från Boost biblioteket.

## Se också

- [C++ Documentation: tolower()](https://www.cplusplus.com/reference/cctype/tolower/)
- [C++ Documentation: transform()](https://www.cplusplus.com/reference/algorithm/transform/)
- [Boost Library: to_lower()](https://www.boost.org/doc/libs/1_77_0/doc/html/string_algo/reference.html#header_boost_algorithm_string_h)
- [Stack Overflow: Convert to Lower Case in C++](https://stackoverflow.com/questions/313970/how-to-convert-stdstring-to-lower-case)