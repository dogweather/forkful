---
title:                "Att Göra En Sträng Stor Begynnelsebokstav"
html_title:           "C#: Att Göra En Sträng Stor Begynnelsebokstav"
simple_title:         "Att Göra En Sträng Stor Begynnelsebokstav"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kapitalisera en sträng är en vanligt förekommande uppgift inom programmering, eftersom det kan göra din kod mer läsbar och förbättra användarupplevelsen i till exempel textbaserade applikationer.

## Hur man gör

För att kapitalisera en sträng i C# kan man använda sig av den inbyggda funktionen "ToUpper", som omvandlar alla bokstäver i en sträng till versaler. Här är ett exempel på hur man kan använda det i en enkel konsolapplikation:

```C#
string input = "hej och välkommen";
Console.WriteLine(input.ToUpper());
```

Detta kommer att producera följande utmatning:

```
HEJ OCH VÄLKOMMEN
```

Om du bara vill kapitalisera första bokstaven i en sträng kan du använda "ToUpper" i kombination med "Substring" för att ta bort resten av strängen efter den första bokstaven. Här är ett exempel:

```C#
string input = "god morgon";
Console.WriteLine(input.Substring(0, 1).ToUpper() + input.Substring(1));
```

Detta kommer att producera utmatningen "God morgon".

## Fördjupning

En viktig sak att notera är att "ToUpper" bara konverterar bokstäver baserat på deras ASCII-värden, vilket innebär att vissa bokstäver i icke-latinska språk kanske inte konverteras till versaler på rätt sätt. Om du behöver hantera detta kan du använda dig av "ToUpperInvariant", som använder en mer generell algoritm för att omvandla versaler.

Det är också viktigt att komma ihåg att "ToUpper" och "ToUpperInvariant" endast skapar en ny sträng med den kapitaliserade versionen, och inte ändrar den ursprungliga strängen. Om du vill uppdatera den befintliga strängen måste du tilldela resultatet av funktionen tillbaka till strängen.

## Se också

- [MSDN Documentation for ToUpper (C# Reference)](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [MSDN Documentation for Substring (C# Reference)](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)