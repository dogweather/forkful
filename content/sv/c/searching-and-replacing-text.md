---
title:                "Sökning och ersättning av text"
html_title:           "C: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Söka och ersätta text är en vanlig uppgift för programmerare. Det handlar helt enkelt om att leta efter en viss bit text och sedan ersätta den med något annat.

Varför gör vi det här? För det första kan det vara ett enkelt sätt att ändra flera förekomster av en viss text eller kod på en gång. Det kan också vara ett sätt att fixa fel eller uppdatera gammal kod.

# Hur?

Här är två enkla sätt att utföra söka och ersätta i C-programmering:

```
// Kodexempel 1:
char text[] = "Hej, världen!";
char sök[] = "världen";
char ersätt[] = "universum";

// Användning av funktionen strstr:
char *ptr = strstr(text, sök); // hitta positionen av "världen" i text
strcpy(ptr, ersätt); // ersätt "världen" med "universum"

printf("%s", text); // Skriver ut "Hej, universum!"

```
```
// Kodexempel 2:
#include <stdio.h>
#include <string.h>

int main(){
  char text[] = "Hello, world!";
  char sök[] = "Hello";
  char ersätt[] = "Goodbye";

  // Användning av funktionen strtok:
  char *ptr = strtok(text, sök);
  strcat(text, ersätt); // Lägger till "Goodbye" till slutet av strängen

  printf("%s", text); // Skriver ut "Goodbye, world!"
  return 0;
}
```

# Djupdykning

Sök- och ersättningsfunktioner började som en del av Unix-operativsystemet på 1970-talet. Sedan dess har dussintals olika implementationer skapats för olika programmeringsspråk.

Det finns också andra sätt att söka och ersätta text i C, till exempel användning av reguljära uttryck (regex). Det är en mer avancerad metod som kräver regelbundna uttryck och specialfunktioner för att leta och ersätta text.

# Se också

- [strstr documentation](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [strtok documentation](https://www.tutorialspoint.com/c_standard_library/c_function_strtok.htm)
- [Regular Expressions in C](https://www.geeksforgeeks.org/different-ways-iterate-string-c/)