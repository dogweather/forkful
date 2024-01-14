---
title:    "C++: Borttagning av tecken som matchar ett mönster"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Varför

I en programmeringsvärld som ständigt förändras, kan vi ibland behöva ta bort vissa delar av en text som matchar ett visst mönster. Det kan till exempel vara för att förbättra prestandan, för att hantera speciella användare eller för att helt enkelt förbättra läsbarheten på en sida. Oavsett anledning kan det vara en användbar funktion att kunna ta bort tecken som matchar ett visst mönster.

## Hur man gör

En av de enklaste sätten att ta bort tecken från en text som matchar ett visst mönster är genom att använda sig av en enkel C++ funktion. Först och främst måste vi deklarera en sträng där vi ska söka efter tecken som överensstämmer med vårt mönster. Sedan kan vi använda oss av en for-loop tillsammans med en if-sats för att kontrollera varje enskilt tecken i strängen. Om tecknet matchar vårt mönster, tar vi bort det från strängen.

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // Deklarera en sträng att söka igenom
    string text = "Hejhej123hej";

    // Loopa igenom varje tecken i strängen
    for (int i = 0; i < text.length(); i++) {

        // Kontrollera om tecknet matchar vårt mönster
        if (text[i] == 'h' || text[i] == '1') {

            // Ta bort tecknet från strängen
            text.erase(i, 1);
        }
    }

    // Skriv ut resultatet
    cout << text << endl;

    return 0;
}
```

Resultatet av detta program kommer att vara "ejje23ej", där alla "h" och "1" som matchade vårt mönster har tagits bort från strängen.

## Djupdykning

Det finns många olika sätt att ta bort tecken från en sträng som matchar ett visst mönster. Man kan till exempel använda sig av en for-loop som i exemplet ovan, eller så kan man använda sig av inbyggda C++ funktioner som "replace" eller "find_and_replace". Det är viktigt att välja den metod som passar bäst för det specifika problemet man står inför.

I detta exempel använde vi oss av en for-loop för att loopa igenom varje tecken i strängen. Men ibland kan det också vara användbart att använda sig av regex (regular expressions) för att matcha ett mer komplicerat mönster för att ta bort tecknen.

Det är också viktigt att komma ihåg att kontrollera strängen innan man tar bort vissa tecken från den. Om man tar bort tecken på fel sätt kan det leda till felaktiga resultat eller en sönderdelad sträng.

## Se även

- [C++ strängar](https://www.w3schools.com/cpp/cpp_strings.asp)
- [Regex i C++](https://www.boost.org/doc/libs/1_55_0/libs/regex/doc/html/boost_regex/syntax/perl_syntax.html)
- [C++ String Standard Library](https://www.geeksforgeeks.org/c-string-library-functions/)