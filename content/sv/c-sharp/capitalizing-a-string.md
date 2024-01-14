---
title:                "C#: TranslateStor bokstavsättning av en sträng"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att stora bokstäver i en sträng kanske inte verkar som en komplicerad uppgift, men det kan vara användbart i vissa situationer. En stor bokstav i början av en mening är en viktig del av stavning och grammatik, och kapitaliserade namn används ofta i programmering för att hänvisa till variabler eller metoder.

## Så här gör du

Det finns flera sätt att göra ett C# program för att kapitalisera en sträng. Här är två exempel:

```C#
// Skapa en string variabel för att representera vår sträng
string str = "detta är en sträng";

// Använd en loop för att iterera över varje tecken i strängen
// och konvertera det första tecknet till en stor bokstav
for (int i = 0; i < str.Length; i++)
{
    if (i == 0)
    {
        // Använd ToUpper() metoden för att göra första tecknet stort
        str = char.ToUpper(str[0]) + str.Substring(1);
    }
}

// Skriv ut den kapitaliserade strängen
Console.WriteLine(str);
// Output: Detta är en sträng
```

En annan metod är att använda inbyggda metoder som `ToUpper()` och `ToTitleCase()`:

```C#
// Skapa en string variabel för att representera vår sträng
string str = "detta är en sträng";

// Använd ToUpper() metoden för att göra hela strängen stor
str = str.ToUpper();

// Skriv ut den kapitaliserade strängen
Console.WriteLine(str);
// Output: DETTA ÄR EN STRÄNG

// Använd ToTitleCase() metoden för att göra första bokstaven i varje ord stor
str = System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(str);

// Skriv ut den kapitaliserade strängen
Console.WriteLine(str);
// Output: Detta Är En Sträng
```

Som du kan se, finns det flera alternativ beroende på dina specifika behov och preferenser.

## Läs djupare

Att kapitalisera en sträng är bara en liten del av större koncept som innehåller att manipulera strängar i C#. Det finns många inbyggda metoder och funktioner som kan hjälpa dig att åstadkomma komplexa strängmanipuleringar. Det är också viktigt att förstå skillnaderna mellan att använda `ToUpper()` och `ToTitleCase()` eftersom de kan ge olika resultat beroende på språk och kulturella inställningar.

## Se också

- [Microsoft C# Dokumentation: String Operationer](https://docs.microsoft.com/sv-se/dotnet/csharp/programming-guide/strings/)
- [C# String Manipuleringstekniker](https://www.c-sharpcorner.com/article/string-manipulation-in-C-Sharp/)