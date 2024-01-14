---
title:                "C#: Utskrift av felsökningsutdata"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Varför

Det finns många tillfällen i utvecklingsprocessen då det är nödvändigt att felsöka koden för att hitta och åtgärda problem. Att utskriva debug output kan vara ett värdefullt verktyg för att förstå vad som händer i programmet under körning. Genom att skriva ut specifika värden och meddelanden kan du enkelt spåra och identifiera felaktigheter i din kod.

# Så här gör du

För att skriva ut debug output i C# kan du använda dig av Console-klassen. Här är ett enkelt exempel:

```C#
Console.WriteLine("Hej världen!");
```

Koden ovan kommer att skriva ut "Hej världen!" i konsolfönstret när programmet körs. Du kan också skriva ut värden på variabler eller objekt för att få en bättre förståelse för hur de förändras under körning.

En annan användbar funktion är att lägga till breakpoints i koden och använda Console.ReadLine() för att vänta på användarens input vid en viss punkt i programmet. Detta kan hjälpa dig att testa och följa koden steg för steg.

# Djupdykning

Att skriva ut debug output är även användbart när du behöver undersöka prestandaproblem eller jämföra resultat mellan olika implementeringar av samma kod. Genom att logga tiden som det tar för en viss del av koden att köra kan du identifiera flaskhalsar och optimera din kod för bättre prestanda.

Det finns också olika verktyg och bibliotek som kan hjälpa dig att logga debug output på ett mer strukturerat sätt, till exempel Serilog eller NLog. Dessa verktyg kan hjälpa dig att organisera och filtrera dina loggar för att enklare hitta och åtgärda problem.

# Se även

- [Using Debug and Trace in C#](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-with-csharp)
- [Debugging Strategies for C# Developers](https://devblogs.microsoft.com/devops/debugging-strategies-for-c-developers/)
- [Serilog](https://serilog.net/)
- [NLog](https://nlog-project.org/)