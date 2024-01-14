---
title:                "C#: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva kod är en process fylld av utmaningar och ibland kan det uppstå fel och buggar som behöver åtgärdas. En användbar teknik för att identifiera och lösa dessa problem är att använda utskrift av debug-utdata. Detta ger en möjlighet att se vad som händer i koden och varför det händer, vilket sparar tid och gör det lättare att felsöka.

## Så här gör du

För att skriva ut debug-utdata i C#, används metoden `Debug.WriteLine()`. Det här är ett exempel på hur man kan använda denna metod:

```C#
int num1 = 5;
int num2 = 10;
Debug.WriteLine("Resultatet av num1 + num2 är: " + (num1 + num2));
```

Det här kommer att skriva ut följande i debug-fönstret:

```
Resultatet av num1 + num2 är: 15
```

Det går också att använda "placeholders" i strängen för att skriva ut variabler. Till exempel:

```C#
string name = "John";
int age = 25;
Debug.WriteLine("{0} är {1} år gammal.", name, age);
```

Det här kommer att skriva ut följande:

```
John är 25 år gammal.
```

Det finns också andra användbara metoder som `Debug.Print()` och `Console.WriteLine()` för att skriva ut debug-utdata. Skillnaden är att `Debug.Print()` endast skriver ut när debugläget är aktivt, medan `Console.WriteLine()` alltid skriver ut data.

## Fördjupning

Att skriva ut debug-utdata ger en möjlighet att se vad som händer i koden i realtid. Detta är särskilt användbart när du arbetar med stora och komplicerade program. Genom att placera utskrift av debug-utdata vid olika steg i koden kan du enklare upptäcka var problem uppstår och varför de uppstår.

Det finns också andra tekniker som kan förbättra användbarheten för utskrift av debug-utdata. Till exempel kan du använda `#define` för att ställa in ett visst debugläge och bara skriva ut data när detta läge är aktivt. Detta sparar resurser och undviker att skriva ut onödig information när programmet körs i produktionsläge.

En annan fördel med att använda utskrift av debug-utdata är att det även kan fungera som en form av dokumentation. Genom att läsa igenom utskriven debug-utdata kan du få en bättre förståelse för hur koden fungerar och vad olika variabler innehåller.

## Se även

- [Microsofts dokumentation för Debug-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug?view=net-5.0)
- ["Introducing Debug Output in C#" av Shawn Wildermuth](https://wildermuth.com/2016/11/15/Introducing-Debug-Output-in-C-)

Hoppas detta har gett dig en bättre förståelse för hur man kan använda utskrift av debug-utdata i C#. Fortsätt utforska och använda denna teknik för att bli en mer effektiv programmerare!