---
date: 2024-01-20 17:47:13.384572-07:00
description: "How to: C# anv\xE4nder `Length`-egenskapen f\xF6r att f\xE5 en str\xE4\
  ngs l\xE4ngd. Koden \xE4r rak: `minStr\xE4ng.Length`. Enkel som en pl\xE4tt, s\xE5\
  \ h\xE4r ser det ut i praktiken."
lastmod: '2024-03-13T22:44:37.904146-06:00'
model: gpt-4-1106-preview
summary: "C# anv\xE4nder `Length`-egenskapen f\xF6r att f\xE5 en str\xE4ngs l\xE4\
  ngd."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

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
