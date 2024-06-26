---
date: 2024-01-26 01:09:24.306143-07:00
description: "Hur man g\xF6r: T\xE4nk dig att du har kod som skriver ut en h\xE4lsning\
  \ flera g\xE5nger. Utan funktioner \xE4r det r\xF6rigt. Med funktioner \xE4r det\
  \ prydligt."
lastmod: '2024-03-13T22:44:37.918936-06:00'
model: gpt-4-1106-preview
summary: "T\xE4nk dig att du har kod som skriver ut en h\xE4lsning flera g\xE5nger."
title: Att organisera kod i funktioner
weight: 18
---

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
