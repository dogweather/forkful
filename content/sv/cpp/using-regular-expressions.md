---
title:                "Använda reguljära uttryck"
html_title:           "C++: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Om du arbetar med textdata och behöver söka, ersätta eller manipulera texter på ett effektivt sätt, då är regular expressions (regex) ett verktyg du definitivt bör lägga till i din programmeringsverktygslåda.

## Hur man använder det

Användning av regex i C++ är ganska enkelt och det finns flera inbyggda funktioner som gör arbetet enklare. Här är några exempel på hur du kan använda regex i din kod:

```C++
#include <iostream>
#include <regex>

using namespace std;

int main(){
    // Skapa ett regex-objekt för att matcha en specifik sträng
    regex str_regex("hej");

    // Söka efter en matchning i en sträng
    string text = "Hej på dig!";
    smatch match;
    if (regex_search(text, match, str_regex)){
        cout << "Match!" << endl;
    }

    // Ersätta en matchning i en sträng
    string ny_text = regex_replace(text, str_regex, "Hallå");
    cout << ny_text << endl;

    return 0;
}
```
Output:
```
Match!
Hallå på dig!
```

Du kan också använda regex för att hitta specifika mönster i en sträng, till exempel ett telefonnummer eller en e-postadress. Detta är särskilt användbart när du behöver bearbeta stora mängder textdata, som i loggfiler eller databaser.

## En djupare titt på regex

Regex är en syntax för att definiera mönster i textsträngar och används för att söka, ersätta och extrahera data från dessa strängar. Det är ett kraftfullt verktyg som kan användas i många olika programmeringsspråk, inklusive C++. Regex kan också vara användbart för att kontrollera inmatning från användare, till exempel genom att kräva en viss formatering av ett lösenord eller en e-postadress.

Det finns olika meta-tecken som kan användas för att definiera olika mönster i regex. Till exempel kan `.` matcha vilken som helst enskild karaktär, `+` matcha en eller flera gånger ett visst tecken och `?` matcha noll eller en gång. Det kan ta lite tid att lära sig alla dessa meta-tecken och hur man använder dem, men när du väl har förstått dem kommer de att göra ditt arbete med textdata mycket snabbare och effektivare.

## Se även

- [Regex Cheat Sheet](https://dev.to/codr/regular-expressions-cheat-sheet-1846)
- [Official C++ Regex Documentation](https://en.cppreference.com/w/cpp/regex)