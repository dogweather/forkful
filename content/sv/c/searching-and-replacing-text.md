---
title:                "Sökning och ersättning av text"
date:                  2024-01-20T17:57:29.526472-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sökning och ersättning av text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text innebär att hitta specifika ord eller fraser i en textsträng och byta ut dem mot andra ord eller fraser. Programmerare använder detta för att effektivisera kodändringar, hantera data och automatisera textredigering.

## How to:
Följande C-program visar hur man söker och ersätter en delsträng.

```C
#include <stdio.h>
#include <string.h>

void searchReplace(char *str, const char *search, const char *replace) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *temp = str;
    size_t search_len = strlen(search);
    size_t replace_len = strlen(replace);

    while (1) {
        const char *p = strstr(temp, search);

        if (p == NULL) {
            strcpy(insert_point, temp);
            break;
        }

        memcpy(insert_point, temp, p - temp);
        insert_point += p - temp;

        memcpy(insert_point, replace, replace_len);
        insert_point += replace_len;

        temp = p + search_len;
    }

    strcpy(str, buffer);
}

int main() {
    char text[] = "Hej världen! Hej alla!";
    searchReplace(text, "Hej", "Tjena");
    printf("%s\n", text); // Skriver ut: "Tjena världen! Tjena alla!"
    return 0;
}
```

## Deep Dive
Söka och ersätta text är en grundläggande operation som funnits sedan tidiga textredigerare, som ed och sed i UNIX. Implementeringar varierar från enkla strängmanipulationer till komplexa algoritmer med reguljära uttryck. C-programmet ovan använder `strstr` för att hitta förekomsten av strängar och `memcpy` för att bygga den nya strängen. Mer avancerade verktyg kan använda Boyer-Moore eller Knuth-Morris-Pratt-algoritmer för effektivare sökningar.

## See Also
- C Standard Library documentation: https://en.cppreference.com/w/c/string
- Regular expressions in C with regex.h library: https://linux.die.net/man/3/regex
- 'sed' command for stream editing: https://www.gnu.org/software/sed/manual/sed.html
