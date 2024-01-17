---
title:                "Interpolering av en sträng"
html_title:           "Arduino: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Interpolering av en sträng innebär att kombinera en statisk text med variabler för att skapa en dynamisk sträng. Programmerare gör detta för att enkelt kunna anpassa och visa information som kan förändras.

## Hur man:

Arduino gör det lätt att interpolera strängar med hjälp av följande syntax:

```arduino
Serial.println("Välkommen till " + stad + "!");
```

```console
Välkommen till Stockholm!
```

I detta exempel kombineras den statiska texten "Välkommen till" med variabeln "stad". När koden körs, kommer värdet av variabeln att ersätta platsmarkören och visa den aktuella staden.

## Djupdykning:

Interpolering av strängar har funnits sedan början av programmering, men har blivit mer populärt på senare tid på grund av dess enkelhet och flexibilitet. Alternativ till interpolering innefattar att skapa en sträng med flera variabler och sedan använda en funktion för att kombinera dem, vilket kan vara mer tidskrävande.

I Arduino har användningen av "string" biblioteket förenklat uppgiften att interpolera strängar. Det finns också flera inbyggda funktioner som kan användas för att formatera och manipulera strängar.

## Se även:

- [Arduino String referens](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Interpolation i andra programmeringsspråk](https://en.wikipedia.org/wiki/String_interpolation)