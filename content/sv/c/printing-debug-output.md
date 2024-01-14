---
title:                "C: Utskrift av felsökningsspårning"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Felsökning är en viktig del av programmering, och ibland kan det vara svårt att förstå vad som händer i en kod. Att skriva ut debug-utgång kan vara ett värdefullt verktyg för att identifiera problem och förbättra din kod.

## Hur man gör
För att skriva ut debug-utgång i C-programmering, kan du använda funktionen "printf". Det finns olika sätt att använda denna funktion, men här är ett enkelt exempel:

```C
int main() {
    int x = 5;
    printf("Värdet på x är %d\n", x);
}
```

I detta exempel använder vi "printf" för att skriva ut värdet på variabeln "x". Notera att vi använder "%d" för att specificera att vi vill skriva ut ett heltal och "\n" för att lägga till en ny rad efter utskriften.

Output av detta program skulle vara "Värdet på x är 5".

## Djupdykning
När du använder "printf" finns det flera formateringsalternativ du kan använda för att skriva ut olika typer av data. Till exempel kan du använda "%f" för att skriva ut flyttal och "%c" för att skriva ut tecken.

Du kan också använda "printf" för att skriva ut värden från olika datatyper, som strängar och arrayer. Det kan också vara användbart att kombinera "printf" med andra funktioner, som "strlen" för att skriva ut längden på en sträng.

Det finns också andra funktioner som "fprintf" och "sprintf" som erbjuder mer flexibilitet för utskrifter. Det är också möjligt att hänvisa till variabler med hjälp av adressoperatorn "&" för att skriva ut deras adresser istället för värden.

## Se även
- [Debugging with printf() in C](https://www.geeksforgeeks.org/debugging-in-c/)
- [C - printf() function](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm)
- [How to Use Debugging in C](https://www.dummies.com/programming/c/how-to-use-debugging-in-c/)