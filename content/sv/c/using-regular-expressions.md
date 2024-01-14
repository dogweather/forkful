---
title:                "C: Användning av reguljära uttryck"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions är ett kraftfullt verktyg som låter dig söka och manipulera textsträngar på ett precist sätt. Genom att lära dig hur man använder dem kan du effektivisera din kodning och lösa problem som skulle vara svårare att hantera på andra sätt.

## Hur man gör

För att använda regular expressions i C är det första steget att inkludera `regex.h` biblioteket. Sedan kan du använda funktionerna `regcomp()` och `regexec()` för att kompilera och utföra dina uttryck. Ett enkelt exempel skulle kunna se ut såhär:

```C
#include <stdio.h>
#include <regex.h>

int main()
{
    regex_t regex;
    char str[] = "Hej! Vilken underbar dag det är idag.";

    regcomp(&regex, "underbar", 0);
    int match = regexec(&regex, str, 0, NULL, 0);

    if (!match)
    {
        printf("Matchning funnen!\n");
    }
    else
    {
        printf("Matchning inte funnen.\n");
    }

    regfree(&regex);

    return 0;
}
```

I detta exempel använder vi `regcomp()` för att kompilera vårt uttryck, "underbar", och sedan använder vi `regexec()` för att söka efter det i strängen `str`. Om en matchning hittas så skrivs "Matchning funnen!" ut, annars skrivs "Matchning inte funnen." ut.

Man kan också använda andra funktioner som `regerror()` för att hantera eventuella felmeddelanden, samt `regfree()` för att rensa minnet som används för uttrycket. Det finns också flera olika teckenklasser och modifierare som kan användas för att göra uttrycken mer avancerade och exakta.

## Djupdykning

Regular expressions kan vara mycket komplexa och det finns mycket att lära när det gäller att använda dem på ett effektivt sätt. Det kan vara värt att lära sig mer om grundläggande uttryck och vanliga användningsfall, samt andra avancerade funktioner som till exempel att fånga eller ersätta delar av en textsträng.

Det är också bra att hålla sig uppdaterad om eventuella förändringar i syntax eller funktioner, då dessa kan variera mellan olika språk och versioner. Det finns också många online-resurser och böcker som kan hjälpa dig att få en djupare förståelse för hur man använder regular expressions.

## Se även

- [Regex Tutorial](https://regexone.com/)
- [The GNU C Library - Regular Expressions](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Mastering Regular Expressions book](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)