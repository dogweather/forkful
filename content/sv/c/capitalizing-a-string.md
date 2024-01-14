---
title:    "C: Att omvandla en sträng till stora bokstäver"
keywords: ["C"]
---

{{< edit_this_page >}}

## Varför

Att göra en sträng med versaler, eller att "kapitalisera" den, är en vanlig operation inom programmering. Det kan vara användbart för att på ett enkelt sätt ändra utseendet på en sträng eller för att jämföra strängar utan att bry sig om skillnader i versaler och gemener.

## Hur man gör

För att kapitalisera en sträng i C, finns det flera sätt att göra det på. Ett vanligt sätt är att använda en inbyggd funktion som heter `toupper()`. Den tar ett tecken som argument och returnerar samma tecken i versaler om det är en bokstav. Om tecknet redan är en versal kommer funktionen att returnera samma tecken.

Här är ett exempel på hur man kan använda `toupper()` för att kapitalisera en helt sträng:

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    char str[] = "hej på dig";
    int i = 0;

    while(str[i])
    {
        str[i] = toupper(str[i]);
        i++;
    }

    printf("%s\n", str);
    return 0;
}

```

Det här programmet kommer att skriva ut "HEJ PÅ DIG" eftersom alla tecken i strängen har omvandlats till versaler. Om du vill ha en sträng med gemener, kan du istället använda den inbyggda funktionen `tolower()`.

Du kan även skapa en egen funktion för att kapitalisera en sträng. Här är ett exempel på hur en sådan funktion skulle kunna se ut:

```C
#include <stdio.h>

void capitalize(char* str)
{
    int i = 0;
    while(str[i])
    {
        if(str[i] >= 'a' && str[i] <= 'z')
        {
            str[i] = str[i] - 32;
        }
        i++;
    }
}

int main()
{
    char str[] = "hej på dig";

    capitalize(str);
    printf("%s\n", str);

    return 0;
}

```

I det här fallet kommer strängen "hej på dig" att omvandlas till "HEJ PÅ DIG".

## Djupdykning

Att kapitalisera en sträng kan verka som en enkel process, men det finns faktiskt flera saker att tänka på. Till exempel, vad händer om strängen innehåller specialtecken eller siffror? Om vi bara använder funktionen `toupper()` så kommer inte dessa tecken att påverkas och kvarstå som de är.

En annan sak att tänka på är att i vissa språk finns det särskilda tecken som behöver hanteras på ett annat sätt än de vanliga bokstäverna. Därför är det viktigt att ha en noggrann kontroll och testning när man skapar en funktion för att kapitalisera strängar.

## Se även

- [toupper()](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [tolower()](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [C Strings](https://www.programiz.com/c-programming/c-strings)