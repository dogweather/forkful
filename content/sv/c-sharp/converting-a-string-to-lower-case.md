---
title:                "C#: Konvertera en sträng till gemener"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig uppgift inom programmering, och det kan vara användbart i många olika sammanhang. Det kan användas för att jämföra strängar oberoende av deras stor- och småbokstäver, och för att standardisera data innan det sparas i en databas.

## Så här gör du

För att konvertera en sträng till gemener i C# använder man metoden `ToLower()`. Detta är en inbyggd metod som finns tillgänglig för alla strängar i C#, och den returnerar en kopia av strängen med enbart gemena bokstäver. Här är ett enkelt exempel:

```C#
string str = "HELLO WORLD";
Console.WriteLine(str.ToLower());
```

Output:
```
hello world
```

## Djupdykning

Det finns ett par saker att tänka på när man konverterar en sträng till gemener. Först och främst är det viktigt att komma ihåg att metoden `ToLower()` endast konverterar bokstäver i ASCII-teckenuppsättningen. Detta innebär att den inte kommer att fungera korrekt för alla språk, särskilt de som använder specialtecken eller bokstäver utanför ASCII-området.

En annan viktig sak att tänka på är att metoden inte ändrar den ursprungliga strängen, utan bara returnerar en kopia av den konverterade strängen. Om man vill ersätta den ursprungliga strängen måste man använda en tilldelningsoperator, t.ex. `str = str.ToLower()`.

Vill man bara konvertera vissa delar av en sträng till gemener, kan man använda metoden `Substring()` tillsammans med `ToLower()` för att välja ut en del av strängen och konvertera den. Här är ett exempel där vi konverterar första bokstaven till gemener:

```C#
string str = "Hello World";
str = str.Substring(0, 1).ToLower() + str.Substring(1);
Console.WriteLine(str);
```

Output:
```
hello World
```

## Se även

- Microsoft C# Dokumentation: https://docs.microsoft.com/sv-se/dotnet/csharp/
- ASCII-tabell: https://www.ascii-code.com/