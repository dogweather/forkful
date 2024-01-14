---
title:    "C++: Användning av reguljära uttryck"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions, eller regex, är ett kraftfullt verktyg för att söka och manipulera textsträngar inom programmering. Det är speciellt användbart för att hantera stora mängder data eller för att snabbt och effektivt söka efter specifika mönster i texten. Det kan vara en avgörande del av ditt programmeringsverktyg när det kommer till att hantera strängar och data på ett effektivt sätt.

## Så här gör du

Att använda regular expressions i C++ är en relativt enkel process. Du använder dig av standardbiblioteket "regex" och inkluderar det i ditt program genom att lägga till ```#include <regex>``` i början av din kod. Sedan kan du börja använda regex-funktioner för att söka efter mönster och manipulera textsträngar.

Ett vanligt sätt att använda regex är för att validera inmatad data. Till exempel, om du vill kontrollera att en e-postadress är giltig kan du använda en regex-mall som matchar standarden för e-postadresser. Här är ett exempel i C++:

```C++
#include <iostream> 
#include <regex> 

int main() 
{  
    std::string email; 
    std::cout << "Skriv in en e-postadress: "; 
    std::cin >> email; 
    std::regex e("(\\w+)(\\.|_)?(\\w*)@(\\w+)(\\.(\\w+))+"); 
    if (std::regex_match(email, e)) { 
        std::cout << "Giltig e-postadress" << std::endl; 
    } else { 
        std::cout << "Ogiltig e-postadress" << std::endl; 
    } 
    return 0; 
} 
```

I det här exemplet använder vi std::regex_match för att matcha inmatad text med en given regex-mall. Om inmatningen inte matchar mallen så är e-postadressen ogiltig och vi skriver ut ett meddelande om det. Om inmatningen matchar så skriver vi ut att det är en giltig e-postadress.

Det finns också andra funktioner som std::regex_search och std::regex_replace som gör det möjligt att söka efter mönster och ersätta dem med annan text.

## Djupdykning

Det finns många olika metakaraktärer och specialtecken som du kan använda i regex för att söka efter specifika mönster. Till exempel används ```\d``` för att matcha siffror och ```\w``` för att matcha bokstäver och siffror. Det finns också mönster som kan matcha specifika ord eller tecken och mönster som kan matcha en viss upprepning av tecken.

Det finns också möjlighet att använda olika flaggor för att ändra hur en regex-mall söker efter ett mönster. Till exempel kan du använda flaggan ```std::regex_constants::icase``` för att ignorera skillnaden mellan stora och små bokstäver vid sökningen.

## Se även

Här är några användbara resurser för att lära dig mer om regular expressions i C++:

- [C++ Reference: Regular Expressions](https://en.cppreference.com/w/cpp/regex)
- [Regex Tutorial av LearnCpp.com](https://www.learncpp.com/cpp-tutorial/regular-expressions/)
- [Regex Cheat Sheet av Programiz.com](https://www.programiz.com/cpp-programming/regex-cheat-sheet)