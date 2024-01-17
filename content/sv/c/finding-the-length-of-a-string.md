---
title:                "Att hitta längden på en sträng"
html_title:           "C: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att hitta längden på en sträng är en vanlig uppgift för programmerare, där man bestämmer hur många tecken en sträng innehåller. Detta är användbart för att hantera strängar på rätt sätt och utföra olika operationer på dem.

## Så här gör du: 
Här är ett exempel på hur du kan hitta längden på en sträng i C:

```
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hej världen!";
    int len = strlen(str);
    printf("Längden på strängen är %d", len);
    return 0;
}

```
Output: Längden på strängen är 13

Vad händer här? Först inkluderar vi "string.h" biblioteket som ger oss funktionen strlen() för att hitta längden på strängar. Sedan definierar vi en sträng "str" med värdet "Hej världen!" och tilldelar sedan längden på strängen till variabeln "len" med hjälp av strlen() funktionen. Slutligen använder vi printf() funktionen för att skriva ut längden på strängen.

## Djupdykning: 
Historiskt sett fanns det ingen inbyggd funktion för att hitta längden på en sträng i C. Istället använde man en while-loop för att iterera genom varje tecken i strängen tills man nådde slutet och räknade därmed antalet tecken. Men med tiden, efterfrågan på datorprestanda och effektivitet ökade, och därför implementerades strlen() funktionen i biblioteket "string.h".

En alternativ metod för att hitta längden på en sträng är med hjälp av funktionen sizeof(), som returnerar storleken i bytes av ett objekt. Dock tar denna metod med i beräkning andra tecken som nollbyten och specialtecken, vilket inte ger en korrekt längd på strängen.

En intressant detalj att notera är att variabeln som lagrar längden av en sträng är av typen "int", vilket betyder att den faktiskt kan bli negativ. Detta på grund av att en negativ siffra också kan ha en vald betydelse när det gäller hantering av tecken.

## Se även: 
- [String Length in C](https://www.geeksforgeeks.org/c-program-find-length-string-without-using-strlen/) 
- [Comparing Strings in C](https://www.programiz.com/c-programming/library-function/string.h/strncmp)