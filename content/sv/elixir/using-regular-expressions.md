---
title:                "Elixir: Användning av reguljära uttryck"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför man skulle vilja använda sig av reguljära uttryck (regular expressions) i sin Elixir-programmering. Regelbundna uttryck ger en mycket kraftfull och flexibel metod att matcha och manipulera textsträngar. Detta kan vara användbart för att extrahera specifika delar av texten, ersätta delar av det med annan text, eller för att validera indata. På så sätt kan man göra mer avancerade och dynamiska operationer på text, vilket kan underlätta programmeringsarbetet.

## Hur man gör

För att använda reguljära uttryck i Elixir, behöver man först importera "regex" biblioteket. Sedan kan man använda sig av funktionen "Regex.match?" tillsammans med ett reguljärt uttryck som första argument och en textsträng som andra argument. Om matchningen lyckas, returnerar funktionen en "Matchdata" struktur som innehåller information om den hittade matchningen. Om matchningen misslyckas returneras "nil".

```Elixir
import Regex

Regex.match?(~r/[0-9]+/, "123") # Returnerar %Regex.MatchData{...}
Regex.match?(~r/[0-9]+/, "abc") # Returnerar nil
```

Man kan också använda "Regex.replace" funktionen för att byta ut delar av en textsträng som matchar ett reguljärt uttryck med en specifik text. I följande exempel byter vi ut alla förekomster av "X" i texten med bokstaven "Y".

```Elixir
Regex.replace(~r/X/, "My example", "Y") # Returnerar "My Yyample"
```

## Djupdykning

Att behärska reguljära uttryck kan vara en viktig färdighet för en programutvecklare, speciellt i webbutveckling där man hanterar mycket textsträngar. För att lära sig mer om reguljära uttryck i Elixir, kan man konsultera den officiella dokumentationen för "Regex" biblioteket och experimentera med olika reguljära uttryck på en webbplats som regex101.com. Det finns också många online-tutorials och böcker som fokuserar på reguljära uttryck och hur man använder dem i programmering.

## Se även

För mer information om reguljära uttryck i Elixir, kolla in dessa resurser:

- Officiella dokumentationen för "Regex" biblioteket: https://hexdocs.pm/elixir/Regex.html
- Regex101.com: en online-webbplats för att testa reguljära uttryck med realtids-feedback: https://regex101.com/
- Elixir School: en gratis webbplats med många lektioner om Elixir, inklusive hur man använder reguljära uttryck: https://elixirschool.com/lessons/basics/regex/