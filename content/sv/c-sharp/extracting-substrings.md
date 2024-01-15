---
title:                "Utvinna delsträngar"
html_title:           "C#: Utvinna delsträngar"
simple_title:         "Utvinna delsträngar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Har du någonsin hamnat i en situation där du behöver separera en del av en textsträng och använda den för ett specifikt syfte? Då kan du behöva lära dig hur man extraherar delsträngar. Det kan vara användbart för att söka efter specifika ord eller teckenkombinationer, dela upp en längre text i mindre delar eller göra textbearbetning.

## Hur man gör det
För att extrahera en delsträng från en befintlig sträng i C#, kan du använda metoden `Substring()` på en `string` variabel. För att använda metoden behöver du först ange den startposition där du vill börja din delsträng och sedan antal tecken som du vill extrahera. Om du till exempel har en sträng `hello world` och vill extrahera `world`, kan du använda koden nedan:

```C#
string originalStr = "hello world";
string extractedStr = originalStr.Substring(6, 5);
Console.WriteLine(extractedStr); // output: world
```

Om du vill extrahera en delsträng från en viss startposition till slutet av en sträng, kan du använda `Substring()` metoden med bara en parameter, vilket kommer att returnera alla tecken från startpositionen till slutet av strängen.

```C#
string originalStr = "adventure";
string extractedStr = originalStr.Substring(3);
Console.WriteLine(extractedStr); // output: enture
```

Om du istället vill extrahera en del av en sträng baserat på specifika tecken, kan du använda metoden `IndexOf()` för att hitta positionen för en viss karaktär eller teckenkombination. Sedan använder du `Substring()` metoden med den hittade positionen för att få önskad delsträng.

```C#
string originalStr = "bunny rabbit";
int index = originalStr.IndexOf("r");
string extractedStr = originalStr.Substring(index);
Console.WriteLine(extractedStr); // output: rabbit
```

## Djupdykning
Det finns också andra användbara metoder för att extrahera delsträngar i C#. Till exempel kan du använda `Split()` metoden för att dela upp en sträng baserat på ett visst tecken eller en teckenkombination. Detta är särskilt användbart om du vill bryta upp en lång sträng i en lista av mindre delar.

```C#
string originalStr = "apple,banana,orange";
string[] fruits = originalStr.Split(',');
Console.WriteLine(fruits[0]); // output: apple
Console.WriteLine(fruits[1]); // output: banana
Console.WriteLine(fruits[2]); // output: orange
```

Du kan också använda `Remove()` metoden för att ta bort en delsträng från en större sträng. Detta är användbart om du till exempel vill ta bort ett prefix eller ett suffix från en sträng.

```C#
string originalStr = "unwanted text";
string newStr = originalStr.Remove(8); // tar bort de första 8 tecknen
Console.WriteLine(newStr); // output: text
```

Sammanfattningsvis är förmågan att extrahera delsträngar en användbar färdighet att ha när du arbetar med textbearbetning i C#. Det finns ett antal olika metoder som du kan använda beroende på ditt specifika syfte och mål.

## Se även
- C# String Class: https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1
- Official C# Documentation: https://docs.microsoft.com/en-us/dotnet/csharp/