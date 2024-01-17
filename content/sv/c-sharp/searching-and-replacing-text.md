---
title:                "Sökning och ersättning av text"
html_title:           "C#: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Sökning och ersättning av text är en viktig del av programmering. Det är en process där vi söker efter en viss bit text och ersätter den med en annan. Detta kan vara användbart för att göra stora ändringar i kod eller för att hitta och korrigera fel.

## Så här gör du:
Här är ett enkelt exempel på hur man kan använda C# för att söka och ersätta text i en sträng:
```
string text = "Hej världen!";
string ersattning = text.Replace("Hej", "Hello");
Console.WriteLine(ersattning);
```
Output:
```
Hello världen!
```

## Djupdykning:
Historiskt sett var sökning och ersättning av text en manuell och tidskrävande process. Men idag kan vi använda olika program eller funktioner inbyggda i våra programmeringsspråk för att göra detta enklare och snabbare.

Alternativ till att använda `Replace` -funktionen inkluderar att använda regex-uttryck eller att använda en loop för att söka efter och ersätta text i en sträng. Implementeringen av dessa alternativ kan dock vara mer komplicerad.

## Se även:
- [C# - String.Replace() - Dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [C# Regular Expressions - Tutorial](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [C# Loops - Tutorial](https://www.tutorialspoint.com/csharp/csharp_loops.htm)