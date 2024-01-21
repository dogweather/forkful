---
title:                "Ta bort tecken som matchar ett mönster"
date:                  2024-01-20T17:41:47.616169-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett mönster innebär att filtrera en textsträng genom att systematiskt ta bort specifika tecken eller sekvenser. Programmerare gör detta för att rensa data, validera inmatning eller förbereda strängar för vidare bearbetning.

## Hur gör man:
Ta bort alla vokaler från en sträng kan se ut som följande i C:

```c
#include <stdio.h>
#include <string.h>

void remove_pattern(char *str, const char *pattern) {
    char *src = str, *dst = str;
    while (*src) {
        const char *temp_pat = pattern;
        while (*temp_pat && *src != *temp_pat) temp_pat++;
        if (!*temp_pat) *dst++ = *src;
        src++;
    }
    *dst = '\0';
}

int main() {
    char my_string[] = "Hej alla programmerare!";
    remove_pattern(my_string, "aeiouyåäöAEIOUYÅÄÖ");
    printf("Resultat: %s\n", my_string);
    return 0;
}
```
Resultatet av koden blir:
```
Resultat: Hj ll prgrmmrr!
```

## Djupdykning
Funktionen `remove_pattern` är en enkel implementering för att ta bort tecken. I tidiga versioner av C fanns det ingen inbyggd standardfunktion för detta specifika behov, vilket innebar att varje utvecklare behövde skapa egna lösningar. Alternativ till egen kodning kan vara att använda befintliga bibliotek som `regex.h` för att matcha och bearbeta mönster. Implementationen ovan använder två pekare för att effektivt gå igenom strängen: en pekare `src` som läser igenom originalsträngen och en `dst` som pekar på nästa position där ett "godkänt" tecken ska placeras. Detta är snabbt eftersom det minimerar antal skrivoperationer till minnet.

## Se även
För dig som vill dyka djupare, här är några resurser:

- C Standard Library documentation: https://en.cppreference.com/w/c/header
- Regular Expressions in C with `regex.h`: https://pubs.opengroup.org/onlinepubs/7908799/xsh/regex.h.html
- Strängbearbetning och mönstermatchning i C (bok): "Mastering Algorithms with C" av Kyle Loudon