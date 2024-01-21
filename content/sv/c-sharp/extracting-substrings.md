---
title:                "Extrahera delsträngar"
date:                  2024-01-20T17:45:33.968163-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
I programmering är 'substring' en delsträng från en annan sträng. Vi extraherar substrings för att manövrera specifika textsegment – till exempel, för datahantering, validering eller sökfunktionalitet.

## Hur gör man:
```C#
string helaStrangen = "Hej Världen!";
string delStrang = helaStrangen.Substring(4, 6); // Väljer "Värld"

Console.WriteLine(delStrang); // Skriver ut: Värld
```

Extraktion med `Span<T>` för mindre minnesanvändning:
```C#
string helaStrangen = "Hej Världen!";
ReadOnlySpan<char> spanStrang = helaStrangen.AsSpan().Slice(4, 6);

Console.WriteLine(spanStrang.ToString()); // Skriver ut: Värld
```

## Fördjupning
I de tidiga dagarna av C# var `Substring`-metoden det mest raka sättet att få en delsträng. Nu finns `Span<T>` och `Memory<T>`, som tillåter mer minneseffektiv hantering av stora strängar och dataströmmar. `Substring` skapar en ny sträng och använder mer minne, medan `Span<T>` ger en vy över den befintliga datan utan att kopiera den.

Strängmanipulation är resurskrävande, så välj rätt verktyg för uppgiften. `Substring` är tillräckligt för enkla och små strängar, medan `Span<T>` är bra när prestanda är kritisk.

## Se även
- Microsofts dokumentation om `String.Substring`: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- En artikel om optimering med `Span<T>`: https://docs.microsoft.com/en-us/archive/msdn-magazine/2018/january/csharp-all-about-span-exploring-a-new-net-mainstay