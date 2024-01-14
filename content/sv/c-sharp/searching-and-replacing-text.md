---
title:                "C#: Söka och ersätta text"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Många programmerare stöter på situationer där de behöver byta ut en viss text i kod eller andra textfiler. Det kan vara på grund av stavningsfel, ändringar i krav eller bara för att göra koden mer läsbar. Sökning och ersättning av text är ett enkelt och effektivt sätt att snabbt göra ändringar i stora mängder text.

## Så gör du
För att söka och ersätta text i C# kan du använda metoden "Replace" i klassen "String". Nedan följer ett exempel på hur du kan använda denna metod:

```C#
// Skapar en sträng att söka och ersätta i
string text = "Hej och välkommen till mitt program!";
// Söker efter ordet "välkommen" och ersätter det med "bye"
string newText = text.Replace("välkommen", "bye");
// Skriver ut den nya strängen
Console.WriteLine(newText);

// Output: Hej och bye till mitt program!
```

I detta exempel söker vi efter ordet "välkommen" i strängen och ersätter det med ordet "bye". Resultatet skriver vi sedan ut i konsolen. Det är viktigt att komma ihåg att metoden "Replace" söker och ersätter exakt matchande text. Om vi bara hade sökt efter "välkommen" skulle inte ordet "välkommen" i början av strängen ha blivit ersatt.

## Djupdykning
Metoden "Replace" har flera olika överlagringar som gör det möjligt att närmare specificera vilken text som ska sökas och ersättas. Till exempel kan man ange en startposition för sökningen, ange en begränsning på antalet ersättningar eller använda en annan klass som implementerar gränssnittet "IEqualityComparer" för att definiera hur jämförelser av text ska utföras.

Det finns också andra sätt att söka och ersätta text i C#, som att använda klassen "Regex" i namespace "System.Text.RegularExpressions". Detta är mer avancerat och erbjuder mer avancerade möjligheter, till exempel mönstermatchning och olika slag av ersättningar. Så om du behöver göra mer avancerade sök- och ersättningsoperationer, kan det vara värt att undersöka denna klass.

## Se även
- [String.Replace Method (Microsoft Documentation)](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [Regex Class (Microsoft Documentation)](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)