---
title:                "C++: Omvandla en sträng till gemener"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener (lower case) kan vara användbart i olika situationer inom programmering, som till exempel när man vill jämföra strängar utan att ta hänsyn till storleksskillnader, eller när man vill standardisera inmatade användardata.

## Hur man gör

En enkel och smidig metod för att konvertera en sträng till gemener är genom att använda sig av den inbyggda funktionen `tolower()` tillsammans med en `for`-loop som loopar över varje tecken i strängen och använder `tolower()` för att byta ut eventuella versaler till gemener.

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // Input sträng som ska konverteras
    string str = "Hej på dig";

    // Loopar över varje tecken i strängen
    for (int i = 0; i < str.length(); i++) {
        // Om nuvarande tecken är en versal, byt ut det med en gemen
        str[i] = tolower(str[i]);
    }

    // Skriver ut den konverterade strängen
    cout << str; // hej på dig

    return 0;
}
```

En annan metod är att använda den inbyggda funktionen `transform()` tillsammans med `tolower()` och `begin()` samt `end()` för att konvertera hela strängen till gemener på en rad.

```C++
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;

int main() {
    // Input sträng som ska konverteras
    string str = "Hallå där";

    // Använder transform() för att konvertera hela strängen
    transform(str.begin(), str.end(), str.begin(), ::tolower);

    // Skriver ut den konverterade strängen
    cout << str; // hallå där

    return 0;
}
```

## Djupdykning

I C++ finns det flera inbyggda funktioner och metoder som kan vara användbara för att manipulera strängar till gemener. En sådan funktion är `toupper()` som istället konverterar en sträng till versaler, och `isalpha()` som kollar om ett tecken är en bokstav eller inte. Det kan även vara värt att titta närmare på hur olika teckenuppsättningar och teckenkodning påverkar konvertering av strängar till gemener.

## Se även

- C++ String Methods: https://www.w3schools.com/cpp/cpp_strings.asp
- ASCII table: https://ascii.cl/
- Unicode and UTF-8 Explanation: https://www.ibm.com/support/knowledgecenter/ssw_aix_72/com.ibm.aix.nlsgdrf/utf-8.htm