---
title:                "Radera tecken som matchar ett mönster"
html_title:           "C#: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför
Ibland när man jobbar med textsträngar kan det hända att man behöver ta bort vissa tecken som matchar ett visst mönster. Det kan till exempel vara för att rensa upp data eller för att manipulera texten på ett önskat sätt.

## Hur man gör det
För att ta bort tecken som matchar ett visst mönster i C#, används metoden `Regex.Replace()`. Den tar in tre parametrar: den ursprungliga textsträngen, mönstret man vill matcha och den nya strängen man vill ersätta matchningen med.

```C#
string originalStr = "Hej123världen";
string modifiedStr = Regex.Replace(originalStr, "[0-9]", "");
Console.WriteLine(modifiedStr); // Kommer att skriva ut "Hejvärlden"
```

Här har vi angett mönstret `"[0-9]"` som matchar alla siffror i textsträngen. Vi har sedan ersatt dem med en tom sträng, vilket i princip tar bort dem från den ursprungliga strängen. Detta är ett enkelt exempel, men man kan använda mer komplicerade mönster för att göra mer avancerade ändringar i texten.

## Djupdykning
Metoden `Regex.Replace()` använder sig av reguljära uttryck (regular expressions) för att matcha mönster i textsträngen. Detta är ett mycket kraftfullt verktyg som låter oss göra avancerade sökningar och manipulationer i text. Reguljära uttryck är dock också väldigt komplicerade och kräver viss kunskap för att använda dem effektivt.

En sak att tänka på när man använder `Regex.Replace()` är att den returnerar en ny sträng, den ändrar inte den ursprungliga strängen. Om man vill behålla ändringarna måste man tilldela resultatet till en variabel. Om man bara vill byta ut en del av den ursprungliga strängen kan man använda `"$"` följt av numret på delen man vill behålla i den nya strängen.

## Se även
Här är några länkar till andra artiklar som kan vara användbara när man arbetar med reguljära uttryck och `Regex.Replace()` i C#:
- [Microsoft Docs - Regex.Replace](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)
- [W3Schools - RegEx Tutorial](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [C# Corner - Regular Expressions in C#](https://www.c-sharpcorner.com/article/the-power-of-regular-expressions-in-c-sharp/)