---
title:                "Stor bokstavsättning av en sträng"
html_title:           "C: Stor bokstavsättning av en sträng"
simple_title:         "Stor bokstavsättning av en sträng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att göra första bokstaven i en sträng stor bokstav, eller "capitalize" som det heter på engelska, kan vara användbart i många olika situationer. Det kan öka läsbarheten av koden och hjälpa till att skapa en enhetlig stil.

## Så här gör du

För att capslockera en sträng i C-programmering, kan du använda funktionen "toupper" från "ctype.h" biblioteket. Här är ett exempel på hur du kan använda den i din kod:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[] = "hej alla!";
  
  for (int i = 0; i < sizeof(str); i++) {
    str[i] = toupper(str[i]);
  }

  printf("%s", str);
  
  return 0;
}
```

Output:

```
HEJ ALLA!
```

## Djupdykning

När man använder "toupper" funktionen, är det viktigt att notera att den bara fungerar för engelska alfabetet. För att capslockera strängar med andra tecken, måste du använda en annan metod, som att konvertera tecknen till deras ASCII kod och sedan manipulera dem.

## Se också

- [`toupper` funktionen i C](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [ASCII tabell](https://www.asciitable.com/)