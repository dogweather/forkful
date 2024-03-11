---
date: 2024-01-20 17:45:33.968163-07:00
description: "I programmering \xE4r 'substring' en delstr\xE4ng fr\xE5n en annan str\xE4\
  ng. Vi extraherar substrings f\xF6r att man\xF6vrera specifika textsegment \u2013\
  \ till exempel, f\xF6r\u2026"
lastmod: '2024-03-11T00:14:11.262229-06:00'
model: gpt-4-1106-preview
summary: "I programmering \xE4r 'substring' en delstr\xE4ng fr\xE5n en annan str\xE4\
  ng. Vi extraherar substrings f\xF6r att man\xF6vrera specifika textsegment \u2013\
  \ till exempel, f\xF6r\u2026"
title: "Extrahera delstr\xE4ngar"
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
