---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till gemener innebär att ändra alla dess versala tecken till gemener. Programmerare gör detta för att ska kunna jämföra strängar oavsett om de är skrivna med stora eller små bokstäver.

## Så här gör du:

Här är en enkel funktion för att konvertera en sträng till gemener i C.

```C
#include <ctype.h> 
#include <stdio.h> 

void tillGemener(char *str) 
{ 
   for(int i = 0; str[i]; i++){ 
      str[i] = tolower(str[i]); 
   } 
} 

int main() 
{ 
   char str[] = "HeJ SaMMaRlUnDa"; 
   tillGemener(str); 
   printf("%s\n", str); 
   return 0; 
} 
```

Output:

```C
hej sammarlunda
```

## Fördjupning:

Att konvertera en sträng till gemener är inget nytt i programmering. Det har varit en nyttig funktion som funnits sedan de tidiga dagarna av programmeringsspråk. 

En alternativ metod till`tolower()`i C är att manipulera ASCII-värdet av bokstäverna. Ascii-värdet för varje versal bokstav är 32 enheter mindre än dess gemena motsvarighet. 

Detaljerna i implementationen varierar beroende på vilket programmeringsspråk du använder. I C, `tolower()`funciton är en del av `ctype.h` bibliotek. Det lopp genom varje tecken i strängen och omvandla det om det är en versal bokstav.

## Se även:

För mer information, kolla in de här nödvändiga läsningarna:

1. [C Library - <ctype.h>](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)
2. [C - Strings](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
3. [C Programming/Strings](https://en.wikibooks.org/wiki/C_Programming/Strings)