---
title:                "C: Sammanslagning av strängar"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

När vi programmerar i C språket, finns det ofta behov av att sammanslå flera strängar till en enda sträng. Detta kan vara användbart för att skapa längre och mer dynamiska strängar, till exempel för utskrifter eller meddelanden till användarna. Att kunna hantera och manipulera strängar på ett effektivt sätt är en viktig del av programmering i C.

## Hur man gör

För att sammanslå strängar i C, använder vi en funktion som heter `strcat()`. Den här funktionen finns i standardbiblioteket `string.h` och tar in två argument: en destinationsträng och en källsträng. Den här funktionen sätter samman källsträngen i slutet av destinationsträngen och returnerar en pekare till den resulterande strängen.

Låt oss titta på ett exempel där vi sammanslår två strängar "Hej" och "världen":

```C
#include <stdio.h>
#include <string.h>

int main() {
  char dest[20] = "Hej ";
  char src[10] = "världen";
  
  strcat(dest, src);
  
  printf("%s", dest);
  return 0;
}
```

Output:

```
Hej världen
```

Vi kan också använda `strcat()` för att sammanslå flera strängar. Detta görs genom att sätta `strcat()`-anropen inuti en `for`-loop eller genom att upprepa anropet för varje sträng som ska sammanslås.

## Grundlig genomgång

Det är viktigt att notera att `strcat()` endast fungerar för att sammanslå nollterminerade strängar. Om antingen destinationsträngen eller källsträngen inte har en giltig nollterminering, kan det leda till oförutsägbara resultat. Det är också viktigt att se till att destinationsträngen har tillräckligt med utrymme för att rymma den resulterande sammanslagna strängen.

Det finns också en annan funktion som heter `strncat()` som liknar `strcat()`, men tar ett tredje argument som bestämmer maximalt antal tecken från källsträngen som ska sammanslås.

## Se också

- [String concatenation in C (GeeksforGeeks)](https://www.geeksforgeeks.org/concatenate-strings-in-c-3-different-ways/)
- [strcat() function in C (Tutorialspoint)](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [String handling in C (W3Schools)](https://www.w3schools.in/c-tutorial/string-handling/)
- [string.h (C Library Functions)](https://www.tutorialspoint.com/c_standard_library/string_h.htm)