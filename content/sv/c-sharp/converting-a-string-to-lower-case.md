---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att omvandla en sträng till gemener innebär att ändra alla tecken i en sträng till små bokstäver. Programmerare gör detta för att standardisera och jämföra strängdata på ett konsekvent sätt, oberoende av hur den ursprungligen skrivs ut.

## Hur man gör
Att konvertera en sträng till gemener i C# är relativt rakt på sak. Här är ett exempel:

```C#
string storaBokstäver = "Hej VÄRLDEN";
string småBokstäver = storaBokstäver.ToLower();
Console.WriteLine(småBokstäver); // skriver ut "hej världen"
```

Här är ett annat exempel som tar input från användaren:

```C#
Console.WriteLine("Skriv in något med STORA bokstäver:");
string input = Console.ReadLine();
string Konverterad = input.ToLower();
Console.WriteLine("Din input i små bokstäver är " + Konverterad);
```

## Djup Dykning 
Historiskt sett är konvertering av strängar till små bokstäver en vanlig praxis inom områden som databasförfrågningar och sökmotoroptimering, där standardisering av data är avgörande för att hitta matchande poster.

Alternativt finns det olika metoder i olika programmeringsspråk för att konvertera en sträng till små bokstäver. Men i C# använder du metoden `.ToLower()` som en del av `System.String`-klassen.

För att titta på implementeringsdetaljer involverar ".ToLower()"-metoden att gå igenom varje tecken i en sträng. För varje tecken kolla så om det är stor bokstav, om det är, konverteras den till motsvarande liten bokstav. 

## Se också
För mer information om strängmanipulation i C#, kolla in dessa resurser:
- Microsoft Docs: String.ToLower Method (https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- Microsoft Learn: Working with Strings in C# (https://docs.microsoft.com/en-us/learn/modules/csharp-manipulate-strings/)
- StackOverflow: C# Convert String to Lower Case (https://stackoverflow.com/questions/17994534/c-sharp-convert-string-to-lower-case)