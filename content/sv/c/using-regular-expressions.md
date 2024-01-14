---
title:                "C: Reply: Använda regelbundna uttryck"
simple_title:         "Reply: Använda regelbundna uttryck"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

I dagens moderna programmeringsvärld är det viktigt att kunna uttrycka sig på ett enkelt och effektivt sätt. Reguljära uttryck, eller regular expressions som det kallas på engelska, är ett verktyg som kan hjälpa dig att göra just detta. Med hjälp av reguljära uttryck kan du enkelt söka och manipulera textsträngar, vilket sparar tid och gör koden mer lättläst. 

## Hur man använder reguljära uttryck i C

För att använda reguljära uttryck i din C-kod behöver du inkludera biblioteket "regex.h". Därefter kan du använda regex-funktionerna för att utföra sökningar och manipulationer på dina textsträngar. Nedan följer några enkla exempel på hur du kan använda reguljära uttryck i din C-kod.

```C
#include <regex.h>

int main() {
    regex_t rgx;
    char str[] = "Jag älskar att koda i C!";
    char pattern[] = "koda";

    // Kompilera reguljärt uttryck
    regcomp(&rgx, pattern, 0);

    // Utför sökning
    int match = regexec(&rgx, str, 0, NULL, 0);

    // Skriv ut resultat
    if (match == 0) {
        printf("Hittade matchning för '%s'!\n", pattern);
    } else {
        printf("Ingen matchning för '%s' hittades.\n", pattern);
    }

    // Rensa upp minne
    regfree(&rgx);

    return 0;
}
```

Överstående kod kompilerar ett reguljärt uttryck för att leta efter ordet "koda" i strängen "Jag älskar att koda i C!". Om en matchning hittas skrivs ett meddelande ut. Det är också viktigt att komma ihåg att rensa upp minnet med hjälp av "regfree" efter att du har använt regex-funktionerna.

## En djupdykning i reguljära uttryck

Det finns en hel del mönster och symboler som kan användas i reguljära uttryck för att göra sökningar och manipulationer mer avancerade. Här följer några av de vanligaste symbolerna och dess betydelse:

- `^` - Motsvarar början av en sträng
- `$` - Motsvarar slutet av en sträng
- `.` - Motsvarar en godtycklig enskild karaktär
- `*` - Motsvarar 0 eller fler förekomster av föregående uttryck
- `+` - Motsvarar 1 eller fler förekomster av föregående uttryck
- `?` - Motsvarar 0 eller 1 förekomst av föregående uttryck
- `()` - Skapar en grupp av uttryck som kan refereras till senare

Om du vill lära dig mer om reguljära uttryck och hur du kan använda dem i din C-kod, rekommenderar jag att du kollar på länkarna nedan under "Se också".

## Se också

- Regelbundna uttryck - Wikipedia (https://sv.wikipedia.org/wiki/Regelbundna_uttryck)
- regex.h - C Standard Library (https://en.wikipedia.org/wiki/Standard_C_library#Regular_expression_std.2C_ERE_and_BRE_functions)
- RegExr - Regex-testare och referens (https://regexr.com/)