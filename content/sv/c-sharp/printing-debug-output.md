---
title:                "Utskrift av felhantering"
html_title:           "C#: Utskrift av felhantering"
simple_title:         "Utskrift av felhantering"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Att skriva ut debuggning-utgångar kan vara ett värdefullt verktyg för att felsöka och förbättra din kod. Det kan hjälpa dig att förstå var ditt program går fel och hur olika delar av koden samverkar.

## Hur man gör
Det finns flera sätt att skriva ut debuggning-utgångar i C#, men det vanligaste är att använda metoden `Console.Write()` eller `Console.WriteLine()`. Dessa metoder tar en eller flera argument och skriver ut dem till konsolen. Här är ett enkelt exempel:

```C#
Console.WriteLine("Hello World!");
```

Detta kommer att skriva ut "Hello World!" till konsolen när du kör ditt program. Du kan också skriva ut variabler eller andra uttryck, till exempel:

```C#
int age = 27;
Console.WriteLine("Min ålder är " + age + " år.");
```

Detta kommer att skriva ut "Min ålder är 27 år." när du kör programmet.

Att använda `Console.Write()` kommer att skriva ut en rad och hålla kvar markören vid slutet av raden. Om du vill fortsätta på samma rad kan du använda `Console.Write()` istället och skriva in en separat rad senare i koden.

Det finns också andra metoder och verktyg som kan hjälpa dig att skriva ut debuggning-utgångar, till exempel `Debug.WriteLine()` och Visual Studio Debugging Tools.

## Deep Dive
Vid felsökning kan det vara användbart att skriva ut mer än bara en sträng eller variabel. Du kan använda formateringssträngar för att skriva ut mer komplexa utgångar, till exempel:

```C#
int num1 = 5;
int num2 = 10;
Console.WriteLine($"Summan av {num1} och {num2} är {num1+num2}.");
```

Detta kommer att skriva ut "Summan av 5 och 10 är 15." Formateringssträngar kan vara särskilt användbara när du arbetar med flera variabler och vill ha en mer strukturerad utgång. Du kan läsa mer om formateringssträngar [här](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting).

Det är också möjligt att skriva ut debuggning-utgångar till en fil istället för till konsolen. Detta kan vara användbart om du vill spara utgångarna för senare analys. Du kan göra detta med `Console.SetOut()` metoden och ange en filväg som argument.

## Se även
- [C# Dokumentation om Debuggning](https://docs.microsoft.com/sv-se/dotnet/csharp/programming-guide/debugging/)
- [Visual Studio Debugging Tools](https://docs.microsoft.com/sv-se/visualstudio/debugger/?view=vs-2019)