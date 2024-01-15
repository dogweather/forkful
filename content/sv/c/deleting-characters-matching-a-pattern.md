---
title:                "Borttagning av tecken som matchar ett mönster."
html_title:           "C: Borttagning av tecken som matchar ett mönster."
simple_title:         "Borttagning av tecken som matchar ett mönster."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Ibland kan det vara nödvändigt att ta bort tecken som matchar ett visst mönster från en sträng. Det kan vara till exempel för att säkerställa att inmatade data är korrekt formaterad eller för att filtrera ut viss information från en text.

## Hur man gör det

För att ta bort tecken som matchar ett mönster från en sträng i C, finns det flera olika sätt att göra det, varav några visas nedan.

```C
// Här är en funktion som tar en sträng och tar bort alla tecken som matchar det givna mönstret
void remove_chars(char *str, char pattern){
    int i, j = 0;
    for (i = 0; str[i] != '\0'; i++){
        if (str[i] != pattern){
            str[j++] = str[i];
        }
    }
    str[j] = '\0';
}

// Exempel på hur funktionen kan användas
char str[] = "Hej $varld!";
remove_chars(str, '$'); // str = "Hej vard!"

```
Om du vill ta bort flera olika tecken som matchar olika mönster, kan du använda `strstr()` funktionen som letar efter ett teckenmönster i en sträng och returnerar en pekare till det första förekomsten av mönstret.

```C
// Här är en funktion som tar en sträng och tar bort alla förekomster av teckenmönster som matchar de tecken som finns i "patterns" arrayen
void remove_chars(char *str, char *patterns, int num_patterns){
    int i, j = 0;
    for (i = 0; str[i] != '\0'; i++){
        if (strstr(patterns, str[i]) == NULL){ // om tecknet inte finns i "patterns" arrayen
            str[j++] = str[i];
        }
    }
    str[j] = '\0';
}

// Exempel på hur funktionen kan användas
char str[] = "Hej! $Varld #123";
char patterns[] = "$#";
remove_chars(str, patterns, 3); // str = "Hej! Varld 123"
```

## Djupdykning

För att ta bort tecken som matchar ett mönster i en sträng, kan man använda sig av olika C-funktioner som `strcpy()`, `strcat()` och `strlen()`. Men för att verkligen utföra operationen på ett effektivt sätt, bör man använda funktioner som `memmove()` eller `memchr()` som gör att man kan flytta tecken i minnet och därmed undvika att skapa onödiga kopior av strängen.

## Se även

- [Dokumentation för C-strängar](https://www.programiz.com/c-programming/c-strings)
- [Guide till teckensträngar i C](https://www.cprogramming.com/tutorial/c/lesson9.html)
- [Användbara C-funktioner för strängar](https://www.programiz.com/c-programming/library-function/string.h)