---
title:                "Elixir: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

##Varför

Att extrahera substrängar i Elixir kan vara användbart för att manipulera textsträngar eller jämföra dem med andra strängar. Det kan också hjälpa till att söka efter specifika mönster eller ord inuti en längre sträng.

## Så här gör du

För att extrahera en substräng från en textsträng i Elixir kan du använda funktionen `String.slice/3` eller `String.substr/2`. Här är ett exempel på hur du kan använda dessa funktioner:

```Elixir
textsträng = "Elixir är ett roligt programspråk!"

String.slice(textsträng, 0, 6)
#Output: "Elixir"

String.substr(textsträng, 11, 6)
#Output: "roligt"
```

Funktionen `String.slice/3` tar tre argument: textsträngen, startpositionen och längden på den önskade substrängen. Funktionen `String.substr/2` tar två argument: textsträngen och positionen där substrängen ska börja.

## Fördjupning

För att förstå hur dessa funktioner fungerar djupare är det viktigt att förstå skillnaden mellan `String`-modulen och `Binary`-modulen i Elixir. `String` representerar textsträngar medan `Binary` representerar binära data. En textsträng kan omvandlas till en binär datastruktur med hjälp av funktionen `to_string/1`.

När substrängar extraheras från en binär datastruktur konverteras de automatiskt till textsträngar av Elixir. Detta kan leda till förvirring om man inte förstår skillnaden mellan `String` och `Binary`-modulerna. Därför är det viktigt att vara medveten om denna skillnad när du arbetar med substrängar i Elixir.

## Se också

- (Elixir - String)[https://hexdocs.pm/elixir/String.html]
- (Elixir - Binary)[https://hexdocs.pm/elixir/Binary.html]
- (Elixir School - Strings)[https://elixirschool.com/en/lessons/basics/basics/#strings]
- (Elixir School - Binaries)[https://elixirschool.com/en/lessons/basics/basics/#binaries]