---
title:                "Interpolering av en sträng"
html_title:           "C#: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolera en sträng är en programmeringsteknik som gör det möjligt att skapa en sträng genom att kombinera flera variabler och textsträngar. Detta gör det enklare att skapa dynamiska strängar, till exempel för utskrift eller att skapa banor för filer. Programmmare använder detta för att öka effektiviteten och minska fel vid skapandet av strängar med variabler.

## Hur To:
```C# 
//Skapa en interpolationsträng med hjälp av $-notation
string name = "Lena";
string message = $"Hej {name}, välkommen till vår applikation!";
Console.WriteLine(message);
```
Output: Hej Lena, välkommen till vår applikation!

```C#
//Kombinera flera variabler 
int age = 25;
decimal salary = 50000.50m;
string info = $"Min ålder är {age} och min lön är {salary} per år";
Console.WriteLine(info);
```
Output: Min ålder är 25 och min lön är 50000.50 per år

## Deep Dive:
Interpolering av strängar introducerades i C# 6.0 och används ofta som ett alternativ till den äldre metoden String.Format(). Det är ett enkelt och lättläst sätt att skapa dynamiska strängar och även minska risken för fel. Implementationen sker genom att lägga till ett dollartecken ($) framför strängen och använda variabler eller uttryck inom måsvingar ({ }).

## Se även:
https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated 
https://www.c-sharpcorner.com/article/interpolated-strings-in-C-Sharp-6-0/