---
title:                "Søking og erstatting av tekst"
html_title:           "C: Søking og erstatting av tekst"
simple_title:         "Søking og erstatting av tekst"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Hva & Hvorfor?
Søking og erstatting av tekst er en vanlig praksis blant programmere for å endre eller erstatte spesifikke deler av en tekst. Dette kan for eksempel være å bytte ut et ord med et annet, eller å endre deler av kode for å gjøre den mer effektiv. Det er viktig for programmere å kunne gjøre disse endringene raskt og nøyaktig for å spare tid og sikre at koden fungerer som den skal.

##Hvordan:
```
#include <stdio.h>

int main() {
  char str[] = "Hei alle sammen!";
  char *ptr = str;
  
  while (*ptr != '\0') {
    if (*ptr == 'e') {
      *ptr = 'a'; //erstatter alle forekomster av 'e' med 'a'
    }
    ptr++;
  }
  
  printf("%s", str); //output: Hai alla samm*n!
  return 0;
}

```
I dette eksemplet ser vi hvordan man kan enkelt kan erstatte bokstaven 'e' med 'a' i en gitt tekst ved å bruke en løkke og pekere i C.

##Dypdykk:
Søking og erstatting av tekst har vært en viktig del av programmering siden språket C ble utviklet på 1970-tallet. Siden da har det kommet mange alternative metoder og verktøy for å gjøre dette mer effektivt, som for eksempel Regex (Regular Expressions). Implementasjonen av søk og erstatting kan variere avhengig av programmeringsspråk, men konseptet er det samme.

##Se også:
- [Regular Expressions i C](https://www.geeksforgeeks.org/regular-expressions-in-c/)
- [C programming language på Wikipedia](https://en.wikipedia.org/wiki/C_(programming_language))