---
title:    "C: Sökning och ersättning av text."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en viktig funktion inom programmering som hjälper oss att snabbt och enkelt göra ändringar i stora mängder av text. Det kan vara användbart för att fixa fel i koden eller för att snabbt uppdatera namn och variabler.

## Så här

För att söka och ersätta text i C-programmering, använder vi funktionen "str_replace", som tar in en sträng att söka efter, en sträng att ersätta den med och en sträng att söka igenom. Här är ett exempel som söker efter ordet "hello" och ersätter det med "hej":

```C
#include <stdio.h>
#include <string.h>

int main(void) {
  char str[] = "Hello world!";
  char sök[] = "hello";
  char ersätt[] = "hej";
  
  char *resultat = str_replace(str, sök, ersätt);
  printf("%s", resultat); // Output: Hej world!
  
  return 0;
}
```

## Djupdykning

Vid sökning och ersättning av text är det viktigt att vara medveten om vissa saker. Funktionen "str_replace" är fallkänslig, vilket betyder att den kommer att skilja mellan stora och små bokstäver. Så om vi i exemplet ovan skulle söka efter "Hello" istället för "hello", skulle ingenting ändras eftersom det inte finns en matchning.

Det är också viktigt att vara uppmärksam på att "str_replace" endast ersätter första matchningen av den sökta strängen. Så om vi skulle ha flera upprepningar av "hello" i vår sträng, skulle bara den första ersättas. Om vi vill ersätta alla förekomster, kan vi använda en loop för att upprepa funktionen för varje förekomst.

## Se även

- [Funktionen "str_replace" i C](https://www.tutorialspoint.com/c_standard_library/c_function_str_replace.htm)
- [Mer information om att söka och ersätta text i C](https://www.geeksforgeeks.org/c-program-find-replace-word-string/)