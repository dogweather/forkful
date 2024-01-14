---
title:                "C#: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng kan verka som en enkel uppgift, men det är faktiskt en viktig del av programmering. Att kunna mäta längden på en sträng är användbart för att bearbeta text eller för att kontrollera inmatningar från användare.

## Hur man gör det

Att hitta längden på en sträng är faktiskt ganska enkelt i C#. Det finns en inbyggd metod som heter "Length" som kan användas för att hitta längden på en sträng. Här är ett exempel på hur det kan se ut:

```C#
string text = "Hej på dig!";
Console.WriteLine(text.Length);
```

Output: 12

Som du kan se använde vi "Length" metoden som ett sätt att få längden på strängen "Hej på dig!". I detta fall är längden 12 eftersom det finns 12 tecken i strängen, inklusive mellanslaget.

En annan metod för att mäta längden på en sträng är att använda "Count" metoden. Denna metod räknar antalet tecken i en sträng och ger samma resultat som "Length" metoden. Här är ett exempel som visar hur man kan använda "Count" metoden:

```C#
string text = "Programmering är fantastiskt!";
Console.WriteLine(text.Count());
```

Output: 27

Som du kan se ger båda metoderna samma resultat, men det är viktigt att notera att "Count" metoden också kan användas på andra datastrukturer som listor och arrays.

## Djupdykning

Nu när vi vet hur man hittar längden på en sträng i C#, låt oss ta en titt på vad som händer bakom kulisserna. En sträng i C# är en samling av tecken, och dess längd definieras av antalet tecken i denna samling. När vi använder "Length" eller "Count" metoden räknas varje tecken i strängen, inklusive mellanrum, specialtecken och siffror.

Det är också viktigt att notera att längden på en sträng är baserad på den specifika encoding som används. Till exempel, om en sträng innehåller specialtecken som inte stöds av den valda encodingen, kommer längden att vara annorlunda.

## Se också

- [C# String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [C# String.Count Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.count?view=net-5.0)