---
title:                "C: Ta bort tecken som matchar ett mönster."
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Det finns många tillfällen där man behöver ta bort vissa tecken från en sträng baserat på ett visst mönster. Det kan vara för att förbättra datakvaliteten, för att uppfylla specifika syntaxregler eller bara för att göra bearbetningen enklare.

## Hur man gör det

Det finns flera sätt att ta bort tecken från en sträng i C-programmering, men det mest effektiva sättet är att använda en enkel loop. Här är ett exempel på hur du kan implementera det:

```C
#include <stdio.h>

// Funktion för att ta bort tecken utifrån ett visst mönster
void removeChars(char *str, char *pattern){
    // Loopa igenom alla tecken i strängen
    for(int i = 0; str[i] != '\0'; i++){
        // Kolla om tecknet matchar mönstret
        for(int j = 0; pattern[j] != '\0'; j++){
            // Om matchning hittas, ta bort tecknet
            if(str[i] == pattern[j]){
                int k = i;
                while(str[k] != '\0'){
                    str[k] = str[k+1];
                    k++;
                }
                i--;
            }
        }
    }
}

int main(){
    // Teststräng och mönster
    char str[] = "Hej hej hej";
    char pattern[] = "ej";

    // Anropa funktionen för att ta bort tecken
    removeChars(str, pattern);

    // Skriv ut den uppdaterade strängen
    printf("%s", str);
    return 0;
}
```

Output:

```
H h h
```

## Fördjupning

En viktig sak att tänka på när du tar bort tecken från en sträng är att du måste hantera minnesallokering. Genom att ta bort tecken flyttas resten av tecknen i strängen och det kan orsaka minnesläckor om du inte hanterar det på rätt sätt.

En annan intressant aspekt är hur du kan anpassa funktionen för att ta bort flera förekomster av ett mönster, inte bara en. Det finns också andra tillvägagångssätt, till exempel att använda inbyggda funktioner som `strncpy()` och `strcat()`.

## Se även

- [C String Header File](https://www.programiz.com/c-programming/library-function/string.h)
- [String Manipulation in C](https://www.geeksforgeeks.org/string-manipulation-in-c/#standardFunction)
- [Efficient String Operations in C](https://www.ibm.com/developerworks/aix/tutorials/au-efficientstringoperations/index.html)