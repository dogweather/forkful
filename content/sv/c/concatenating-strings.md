---
title:    "C: Sammanslående strängar"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att kunna kombinera, eller concatenera, strängar är en väsentlig färdighet i C-programmering. Genom att lära sig detta kan du skapa dynamiska och flexibla program som kan hantera variabel längd på inmatade textsträngar. Detta är särskilt användbart när du arbetar med användargränssnitt eller textfiler.

## Så här gör du

### Skapa en ny sträng

För att concatenating av strängar måste du först skapa en ny sträng som har tillräckligt med utrymme för att rymma det slutgiltiga resultatet. Detta gör du med hjälp av `malloc()` funktionen som allokerar minne dynamiskt.

```
char *nySträng = (char*)malloc(100*sizeof(char));
```

I detta exempel allokerar vi utrymme för en sträng med längden 100 tecken. Du kan anpassa storleken efter dina behov.

### Kopiera strängar

Nästa steg är att kopiera de strängar som du vill concatenera in i den nya strängen. Du kan använda `strcpy()` funktionen för att göra detta.

```
char *sträng1 = "Hej";
char *sträng2 = "världen";

strcpy(nySträng, sträng1);
strcpy(nySträng, sträng2);
```

Efter denna operation kommer `nySträng` att innehålla "Hej världen".

### Lägga till ytterligare strängar

Om du vill concatenating fler strängar kan du använda `strcat()` funktionen istället för `strcpy()`. Detta tillåter dig att lägga till en sträng till slutet av en befintlig sträng istället för att ersätta den.

```
strcat(nySträng, "!");
```

I detta exempel kommer utdata att bli "Hej världen!". Du kan fortsätta att lägga till fler strängar på samma sätt.

## Deep Dive

Förutom `strcpy()` och `strcat()` finns det ytterligare funktioner som du kan använda för att concatenera strängar. Dessa inkluderar `strncpy()`, `sprintf()`, och `strncat()`. Var noga med att läsa dokumentationen för varje funktion för att förstå hur de fungerar och vilka begränsningar de har.

Det kan också vara användbart att känna till att C har en inbyggd operator för concatenating strängar, som är `+`. Du kan använda den på samma sätt som du skulle göra med numeriska värden.

## Se också

- [C String Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Dynamic Memory Allocation in C](https://www.geeksforgeeks.org/dynamic-memory-allocation-in-c-using-malloc-calloc-free-and-realloc/)
- [C String Concatenation](https://www.programiz.com/c-programming/library-function/string.h/strncat)