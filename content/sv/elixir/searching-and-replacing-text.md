---
title:                "'Sökning och ersättning av text'"
html_title:           "Elixir: 'Sökning och ersättning av text'"
simple_title:         "'Sökning och ersättning av text'"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text är en vanlig uppgift för programmerare. Det handlar helt enkelt om att hitta en viss text i en fil eller en sträng och ersätta den med en annan. Detta är användbart för att göra massvisa ändringar i kod, eller för att korrigera stavfel eller felaktig formatering.

## Så här:

För att söka och ersätta text i Elixir kan du använda funktionen `String.replace/4`, som tar in fyra argument: en sträng, ett mönster att söka efter, vad som ska ersättas samt hur många gånger det ska ersättas.

```
Elixir
code = "Hej världen, jag är en programmerare."
String.replace(code, "programmerare", "kodare", 1)
# Output: "Hej världen, jag är en kodare."
```

Du kan också använda regex för att söka och ersätta text i en sträng med hjälp av funktionen `Regex.replace/3`.

```
Elixir
code = "Hej världen, jag är en programmerare."
Regex.replace(~r/programmerare/, code, "kodare")
# Output: "Hej världen, jag är en kodare."
```

## Djupdykning:

Sökning och ersättning av text har funnits i många år och är en grundläggande funktion i de flesta programmeringsspråk. I Elixir finns det flera olika funktioner för detta ändamål, såsom `String.replace/4`, `Regex.replace/3` och `String.replace_leading/3`, vilka alla ger olika möjligheter för sök- och ersättningsfunktioner.

En alternativ metod för att söka och ersätta text i Elixir är att använda sig av `String.replace!/4`, där det fjärde argumentet är en modifierare för att ange hur ersättningen ska ske. Mer detaljerade beskrivningar och exempel kan hittas i Elixir-dokumentationen för dessa funktioner.

## Se även:

- Elixir dokumentation för sökning och ersättning av text: https://hexdocs.pm/elixir/String.html#module-replacing-and-removing-substrings