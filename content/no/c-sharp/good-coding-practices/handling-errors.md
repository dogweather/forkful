---
title:                "Feilhåndtering"
aliases:
- /no/c-sharp/handling-errors.md
date:                  2024-01-26T00:50:03.952948-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/handling-errors.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Håndtering av feil i C# dreier seg om å håndtere det uventede—som å snuble i skolissene. Programmer kan snuble over dårlige data eller ustabile tilkoblinger. Vi håndterer feil for å hindre at programvaren vår går på trynet, og lar den heller komme seg elegant videre.

## Hvordan:

La oss starte med en try-catch-blokk. Det er som å sette et sikkerhetsnett under en linedanser. Hvis de sklir, faller de ikke—de blir fanget opp.

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] tall = {1, 2, 3};
            Console.WriteLine(tall[5]);  // Oops, indeksen er utenfor grensene!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("Fanget en feil: " + e.Message);
        }
    }
}
```

Eksempel på utskrift når ting går galt:
```
Fanget en feil: Indeks var utenfor grensene for matrisen.
```

Nå legger vi til en finally-blokk—det er det som skjer uansett, som å betale skatter.

```C#
try {
    // Potensielt problematisk kode her
} catch (SomeSpecificException e) {
    // Håndter den spesifikke feilen her
} finally {
    // Denne koden kjører uansett hva som skjer ovenfor
    Console.WriteLine("Dette kjører alltid.");
}
```

## Dypdykk

Feilhåndtering har vært i C# siden dets fødsel. Over tid har det utviklet seg. Tidligere stolte programmerere på returkoder eller globale flagg for å signalisere problemer—klumpete og feilutsatt.

C# bruker unntak (exceptions), en mer moderne tilnærming. Et unntak kastes når det uventede skjer, akkurat som å kaste et flagg i en fotballkamp. Strukturert unntakshåndtering med try, catch og finally-blokker gjør det enklere og renere å håndtere disse øyeblikkene enn gammeldags feilsjekking.

Alternativer? Sikkert. Det finnes `UnhandledExceptionEventHandler` for unntak som glipper gjennom. Eller i asynkron kode, hvor feilhåndtering blir litt snudd på hodet med `Task` objekter som bærer med seg sine egne unntak.

Implementeringsdetaljer—lik den lille skriften—betyr noe. Unntak kan være kostbare, de drar ned ytelsen hvis de kastes uten videre. Så, vi bruker dem for eksepsjonelle tilfeller, ikke for daglig logikk-kontroll.

## Se Også

- [Offisiell dokumentasjon om unntak i C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [Beste praksiser for unntakshåndtering i C#](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
