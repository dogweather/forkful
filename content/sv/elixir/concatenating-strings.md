---
title:                "Elixir: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Det kan verka som en enkel uppgift, men att sammanslå strängar är en vanlig uppgift inom programmering och kan vara användbar i många olika sammanhang. Genom att förstå hur man konkatenerar strängar i Elixir kan du öka dina kodningskunskaper och skriva mer effektiv och läsbar kod.

## Hur man gör det

För att sammanslå strängar i Elixir kan du använda operatorn `<>`. Det gör att två strängar förenas och skapar en ny sträng. Du kan också använda `String.concat/1` -funktionen som tar en lista av strängar och slår samman dem till en enda sträng. Här är några exempel på hur du kan göra det:

```Elixir
IO.puts "Hello " <> "world!" 
=> Hello world!

String.concat(["Coding", "is", "fun"])
=> Codingisfun
```

Som du kan se ovan måste du använda citationstecken runt de strängar som du vill sammanslå. Om du inte gör det kan du få olika felmeddelanden.

## Djupdykning

En intressant sak att notera är att operatorn `<>` är kommutativ, vilket innebär att ordningen på strängarna inte spelar någon roll. Men funktionen `String.concat/1` är inte kommutativ och ordningen på strängarna spelar en viktig roll. Om vi till exempel tar följande kod:

```Elixir
IO.puts "1" <> 2
=> ** (ArgumentError) argument error

String.concat([1, "2"])
=> 12
```

I det första exemplet får vi ett felmeddelande eftersom vi försöker slå samman en sträng med ett heltal. Men i det andra exemplet konverteras heltalen till strängar och slås sedan samman.

Det är också värt att nämna att det finns andra inbyggda funktioner i Elixir som kan användas för att konkatenera strängar, som `Kernel.to_string/1` eller `Integer.to_string/1`. Det är viktigt att förstå vilken typ av data du arbetar med och välja rätt funktion för att undvika fel.

## Se även

- [Elixir dokumentation om strängar](https://elixir-lang.org/getting-started/strings.html)
- [Elixir School - Strängar](https://elixirschool.com/sv/lessons/basics/strings/)