---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:26.131664-07:00
description: "Att skriva med stor bokstav i en str\xE4ng i C# inneb\xE4r att omvandla\
  \ det f\xF6rsta tecknet i en str\xE4ng till versal om det inte redan \xE4r det.\
  \ Denna \xE4ndring kan\u2026"
lastmod: '2024-02-25T18:49:36.193648-07:00'
model: gpt-4-0125-preview
summary: "Att skriva med stor bokstav i en str\xE4ng i C# inneb\xE4r att omvandla\
  \ det f\xF6rsta tecknet i en str\xE4ng till versal om det inte redan \xE4r det.\
  \ Denna \xE4ndring kan\u2026"
title: "G\xF6r om en str\xE4ng till versaler"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva med stor bokstav i en sträng i C# innebär att omvandla det första tecknet i en sträng till versal om det inte redan är det. Denna ändring kan vara avgörande för att formatera utdata, genomdriva kodningsstandarder eller göra texter i användargränssnittet mer läsliga.

## Hur man gör:
C# erbjuder ett enkelt tillvägagångssätt för att skriva strängar med stor bokstav med inbyggda metoder. Det enklaste sättet att uppnå detta är genom att direkt modifiera strängen med dessa metoder. För mer komplexa eller specifika kapitaliseringsregler (t.ex. att skriva varje ord med stor bokstav) kan ytterligare bibliotek eller manuella metoder vara nödvändiga. Nedan finns exempel som demonstrerar hur man skriver en sträng med stor bokstav på olika sätt i C#.

### Grundläggande kapitalisering:
För att skriva första bokstaven i ett enda ord eller en mening med stor bokstav:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Utdata: "Hello world"
```

### Skriva varje ord med stor bokstav:
För att skriva första bokstaven i varje ord i en sträng med stor bokstav kan du använda metoden `TextInfo.ToTitleCase` som finns i namnrymden `System.Globalization`:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Utdata: "Hello World"
```

Observera: `ToTitleCase` omvandlar inte resten av bokstäverna till gemener; den ändrar endast till versaler den första bokstaven i varje ord. Dessutom kanske vissa ord i regler för titelfall (som "and", "or", "of") inte skrivs med stor bokstav beroende på kulturinställningarna.

### Använda utökningsmetoder för återanvändbarhet:
Du kan skapa en utökningsmetod för `string`-klassen för att förenkla kapitaliseringsprocessen, vilket gör din kod renare och mer återanvändbar. Så här skapar och använder du en sådan metod:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Utdata: "Hello world"
    }
}
```

Denna utökningsmetod `Capitalize` kan anropas på vilket strängobjekt som helst inom namnrymden, vilket erbjuder ett mer intuitivt och objektorienterat tillvägagångssätt för strängmanipulation i C#.

### Tredjepartsbibliotek:
Även om C#s standardbibliotek täcker de flesta behov för att skriva strängar med stor bokstav, kan vissa specialiserade uppgifter dra nytta av tredjepartsbibliotek, som Humanizer. Dock, för uppgiften att enkelt skriva strängar eller varje ord i en sträng med stor bokstav, är standardmetoderna i C# adekvata och effektiva, vilket eliminerar behovet av externa beroenden.
