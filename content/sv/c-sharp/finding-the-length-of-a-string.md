---
title:                "C#: Längden på en sträng"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att hitta längden på en sträng är en grundläggande del av programmering och är användbar för att kontrollera inmatade data samt formatera utdata på ett korrekt sätt.

## Hur man gör
För att hitta längden på en sträng i C# kan du använda metoden `Length` som finns på alla strängar. Det här gör det enkelt att få fram längden på en sträng utan att behöva räkna antalet tecken manuellt.

```C#
string myString = "Hej på dig!";
int stringLength = myString.Length;

Console.WriteLine(stringLength);
```

Detta kommer att skriva ut talet 11, vilket är antalet tecken i strängen.

## Djupdykning
I C# är en sträng en sekvens av Unicode-tecken som lagras i en datastruktur som kallas för `string`. De vanligaste metoderna som används för att manipulera strängar är `Length`, `Substring`, `Trim`, `Replace` och `Split`.

Metoden `Length` används för att få fram längden på en sträng, och den returnerar antalet tecken i strängen. Det är viktigt att komma ihåg att även vita mellanslag och specialtecken räknas som tecken i en sträng.

## Se även
- [Microsoft C# documentation: String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1)
- [W3Schools: C# Strings](https://www.w3schools.com/cs/cs_strings.asp)
- [C# Corner: String Methods in C#](https://www.c-sharpcorner.com/article/string-methods-in-c-sharp/)