---
title:    "Gleam: Extrahera substrings"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Extracting substrings, or extracting a specific portion of a string, can be incredibly useful in various programming tasks. Whether you are working with text parsing, data manipulation, or even in natural language processing, being able to extract substrings can save you time and improve the accuracy of your code. In this article, we will explore how to use the Gleam programming language to extract substrings with ease.

## Hur man gör

Att extrahera substrings i Gleam är enkelt och kan uppnås med hjälp av `String.slice` funktionen. Denna funktion tar in två parametrar: en sträng och en lista som specificerar vilka delar av strängen du vill extrahera.

```Gleam
let name = "Alice Johnson"
let first = String.slice(name, [0..4])    // Output: Alic
let last = String.slice(name, [6..12])    // Output: Johnson
```

I exemplet ovan har vi tilldelat en sträng till variabeln `name` och sedan använt `String.slice` för att extrahera både förnamn och efternamn med hjälp av indexpositioner. Du kan också använda `String.slice` för att extrahera delar av en sträng baserat på specifika villkor, till exempel om en bokstav eller ett visst ord finns i strängen.

```Gleam
let sentence = "Jag älskar att lära mig nya språk."
let loveIndex = String.index_of(sentence, "älskar")       // loveIndex: 4
let learning = String.slice(sentence, [9..-5])            // learning: att lära mig nya språk
```

## Djupdykning

Det finns flera andra funktioner i Gleam som också kan användas för att extrahera substrings. `String.index_of` hjälper till att hitta positionen för en viss del av en sträng, vilket sedan kan användas med `String.slice` för att extrahera den delen. `String.contains` är en annan användbar funktion som returnerar sant eller falskt om en viss del finns i en sträng.

Det är också viktigt att notera att Gleam hanterar unicode-tecken korrekt, vilket gör det enklare att extrahera substrings från icke-engelska språk.

## Se även

- Gleam dokumentation för `String.slice`: https://gleam.run/documentation/stdlib/String.html#slice
- Mer information om string manipulation i Gleam: https://gleam.run/documentation/tutorials/strings.html