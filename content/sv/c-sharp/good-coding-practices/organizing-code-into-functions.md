---
title:                "Att organisera kod i funktioner"
aliases: - /sv/c-sharp/organizing-code-into-functions.md
date:                  2024-01-26T01:09:24.306143-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att organisera kod i funktioner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att dela upp kod i funktioner är som att sortera LEGO-bitar i lådor – det gör det lättare att hitta och använda dem. Vi gör detta för att undvika upprepning, för att förenkla förståelsen, och för att underhållet ska bli mindre av en huvudvärk.

## Hur man gör:
Tänk dig att du har kod som skriver ut en hälsning flera gånger. Utan funktioner är det rörigt. Med funktioner är det prydligt.

```C#
// Utan funktioner - repetitivt
Console.WriteLine("Hej, Amy!");
Console.WriteLine("Hej, Bob!");
Console.WriteLine("Hej, Charlie!");

// Med funktioner - snyggare
void Greet(string name) {
    Console.WriteLine($"Hej, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

Resultatet är detsamma, men den andra versionen är mycket mer välordnad.

## Fördjupning
Långt tillbaka, i assembler-språkets dagar, skulle du hoppa till olika kodställen med GOTO – kaotiskt och svårt att följa. Funktioner är en stor uppgradering, som organiserade lådor i en verktygslåda. Alternativ? Absolut. Du har metoder, vilket är funktioner i ett klass-sammanhang. Sedan finns det lambdas och in-line funktioner för snabba, engångsuppgifter.

När det gäller implementering – små, fokuserade funktioner är guld. De är lättare att testa och felsöka. Stora funktioner med många ansvarsområden kan bli monstruösa och förtjänar det tvivelaktiga namnet "spaghettikod". Håll dig till ett jobb per funktion; du kommer att tacka dig själv senare.

## Se också
För mer om funktioner och bästa praxis, kolla in:

- Clean Code av Robert C. Martin: Principer för att hålla dina funktioner snygga.
- Refactoring av Martin Fowler: Sätt att förbättra befintlig kod.
- Microsoft C# Guide om metoder: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
