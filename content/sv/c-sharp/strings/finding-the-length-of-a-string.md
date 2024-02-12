---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:47:13.384572-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att hitta längden av en sträng innebär att räkna antalet tecken den innehåller. Programmerare behöver detta för att manipulera text, validera inmatning eller bara för att avgöra hur mycket information en sträng bär på.

## How to:
C# använder `Length`-egenskapen för att få en strängs längd. Koden är rak: `minSträng.Length`. Enkel som en plätt, så här ser det ut i praktiken:

```C#
using System;

class Program {
    static void Main() {
        string greeting = "Hej världen!";
        Console.WriteLine(greeting.Length); // 12
    }
}
```

Kör koden, och din output blir `12`, eftersom "Hej världen!" har 12 tecken.

## Deep Dive:
Historiskt sett har stränglängder varit centrala i många programmeringsspråk. I C#, som är ett högnivåspråk, är hanteringen av stränglängder smidig och direkt.

Det finns alternativ till `Length`, till exempel `StringInfo`-klassen för mer komplexa scenarion som att hantera Unicode och kombinerade tecken. Dessa är dock sällan nödvändiga för enkel längdberäkning.

Implementationen av `Length` är effektiv. C# strängar är objekt av klassen `String`, som lagrar längden internt som en `int`. När du begär `.Length`, hämtas helt enkelt detta värde - det räknas inte varje gång.

## See Also:
- Microsoft dokumentation om `String`-klassen: [String Class](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-6.0)
- MSDN artikel om `Length`-egenskapen: [String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-6.0)
