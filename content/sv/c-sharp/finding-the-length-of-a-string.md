---
title:    "C#: Hitta längden på en sträng"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng kan verka som en enkel uppgift, men det är en viktig grundläggande del av programmering. Genom att veta hur man hittar längden på en sträng kan man manipulera och hantera data på ett effektivare sätt, vilket kan leda till bättre och mer strukturerad kod.

## Hur man gör det

För att hitta längden på en sträng i C# kan man använda sig av den inbyggda metoden "Length". Detta gör man genom att skriva "sträng.Length" där "sträng" är namnet på den sträng man vill hitta längden på. Nedan följer ett exempel på hur man kan använda denna metod:

```C#
string namn = "Johan Svensson";
int längd = namn.Length;

Console.WriteLine("Längden på strängen är: " + längd);
```

I exemplet ovan skapar vi en sträng med värdet "Johan Svensson" och tilldelar längden på strängen till variabeln "längd". Därefter skriver vi ut längden på strängen på konsolen. Om man kör detta program får man följande output:

```C#
Längden på strängen är: 14
```

Som man kan se så är värdet på variabeln "längd" 14, vilket är antalet tecken i strängen.

## Djupdykning

När det kommer till att hitta längden på en sträng finns det vissa saker som kan vara bra att tänka på. Till exempel är det viktigt att komma ihåg att längden som returneras av metoden "Length" är antalet tecken i strängen och inte antalet ord. Om man exempelvis har en sträng som innehåller flera ord separerade med mellanslag så kommer inte längden att inkludera mellanslagen. En annan sak man bör vara medveten om är att metoden bara fungerar för stringar och inte för andra datatyper, såsom numeriska data.

## Se även

- [C# - Längd på sträng](https://www.w3schools.com/cs/cs_strings_length.asp)
- [Microsoft Docs - String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1)