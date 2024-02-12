---
title:                "Hantering av fel"
aliases:
- sv/c-sharp/handling-errors.md
date:                  2024-01-26T00:50:43.965968-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hantering av fel"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/handling-errors.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hantera fel i C# handlar om att hantera det oväntade—som att snubbla på sina egna skosnören. Program kan snubbla på dåliga data eller ostadiga anslutningar. Vi hanterar fel för att förhindra att vår mjukvara "ansiktet först" i marken, och låter den återhämta sig anständigt.

## Hur gör man:

Låt oss börja med en try-catch-block. Det är som att sätta ett säkerhetsnät under en lindansare. Om de halkar faller de inte pladask—de fångas upp.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] nummer = {1, 2, 3};
            Console.WriteLine(nummer[5]);  // Hoppsan, index utanför gränserna!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Fångade ett fel: " + e.Message);
        }
    }
}
```

Exempel på utskrift när saker går snett:
```
Fångade ett fel: Index was outside the bounds of the array.
```

Nu lägger vi till ett finally-block—det är det som händer oavsett vad, som att betala skatter.

```C#
try {
    // Potentiellt problematisk kod här
} catch (SomeSpecificException e) {
    // Hantera det specifika felet här
} finally {
    // Denna kod körs oavsett vad som händer ovan
    Console.WriteLine("Detta körs alltid.");
}
```

## Djupdykning

Felhantering har funnits i C# sedan dess födelse. Över tid har det utvecklats. På den gamla goda tiden förlitade sig programmerare på returkoder eller globala flaggor för att signalera problem—klumpigt och felbenäget.

C# använder undantag, en mer modern metod. Ett undantag kastas när något oväntat händer, precis som att kasta en flagga på spelplanen i amerikansk fotboll. Strukturerad undantagshantering med try, catch och finally-block gör hantering av dessa ögonblick klarare och renare än gammaldags felkontroll.

Alternativ? Visst. Det finns `UnhandledExceptionEventHandler` för undantag som glider igenom. Eller i asynkron kod, där felhantering blir lite annorlunda med `Task`-objekt som bär sina egna undantagsbagage.

Implementeringsdetaljer—liknande det finstilta—är viktigt. Undantag kan vara kostsamma, och dra ner prestandan om de kastas hej vilt. Så, vi använder dem för exceptionella fall, inte för daglig logikkontroll.

## Se även

- [Officiell dokumentation om undantag i C#](https://docs.microsoft.com/sv-se/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Bästa praxis för felhantering i C#](https://docs.microsoft.com/sv-se/dotnet/standard/exceptions/best-practices-for-exceptions)
