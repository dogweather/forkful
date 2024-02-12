---
title:                "Interpolera en sträng"
aliases:
- /sv/c-sharp/interpolating-a-string.md
date:                  2024-01-20T17:50:23.371599-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stringinterpolering i C# låter dig smidigt infoga variabler i strängar. Det gör koden mer läslig och underlättar dynamisk textmanipulation.

## Så här gör du:
```
C#
string name = "Anna";
int age = 32;
string greeting = $"Hej {name}, du är {age} år gammal.";
Console.WriteLine(greeting);
```
Utskrift: `Hej Anna, du är 32 år gammal.`

```
C#
double price = 99.50;
int quantity = 3;
string orderMessage = $"Totalt: {quantity} produkter för {price*quantity:C2}.";
Console.WriteLine(orderMessage);
```
Utskrift: `Totalt: 3 produkter för 298,50 kr.`

## Djupdykning:
Stringinterpolering introducerades i C# 6 och används med `$` före citattecknet. Det ersättade `String.Format()`, vilket var mer klumpigt. Till exempel `String.Format("Hej {0}, du är {1} år gammal.", name, age)`. Med stringinterpolering infogas värdena direkt och `{}` innehåller kod som körs vid exekvering. Det är viktigt att notera att C# kompilerar interpolerade strängar till `String.Format()` bakom kulisserna, vilket innebär att prestandan är densamma. Använd `{{` och `}}` för klammerparenteser i det slutliga strängresultatet.

## Se även:
- [Microsofts officiella dokumentation om stringinterpolering](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
