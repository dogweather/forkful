---
title:    "C#: Skriva till standardfel"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför
Att skriva till standardfelutmatningen (standard error) är ett sätt för en programmerare att få diagnosinformation eller felmeddelanden från sitt program. Detta kan vara till stor hjälp vid felsökning och debugging.

## Hur man gör
För att skriva till standardfelutmatningen i C# kan du använda metoden `Console.Error.WriteLine()`. Detta kommer att skriva ut en sträng till standardfelutmatningen. Här är ett exempel på hur det kan se ut i din kod:

```C#
try
{
    // Kod som potentiellt kan orsaka ett fel
}
catch (Exception ex)
{
    Console.Error.WriteLine($"Ett fel inträffade: {ex.Message}");
}
```

I detta exempel skickar vi ett felmeddelande till standardfelutmatningen om det skulle uppstå ett undantag i vårt program. Detta kan hjälpa oss att identifiera och åtgärda problemet.

Det är också viktigt att komma ihåg att standardfelutmatningen är bättre lämpad för felmeddelanden, medan standardutmatningen (standard output) bör användas för vanliga utskrifter. Att blanda samman dessa kan leda till otydlig och svårläst kod.

## Fördjupning
Det finns också andra metoder för att skriva till standardfelutmatningen, såsom `Console.Error.Write()` och `Console.Error.WriteLineAsync()`. Om du vill läsa mer kan du kolla in Microsofts officiella dokumentation på ämnet.

Att lära sig hur man skickar information till standardfelutmatningen är en viktig del av att skriva robust och felsäker kod. Detta gör att du kan få värdefull information om eventuella fel i ditt program och enklare åtgärda dem.

## Se också
- [Console class (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/console-class)
- [Standard Error (stderr)](https://www.gnu.org/software/libc/manual/html_node/Error-Output.html)