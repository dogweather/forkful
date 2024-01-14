---
title:                "Elixir: Extrahering av substringar"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I Elixir-programmering finns det många olika verktyg som kan hjälpa dig att hantera strängar på ett effektivt sätt. Ett av dessa verktyg är möjligheten att extrahera substrängar från en befintlig sträng. Genom att använda denna funktion kan du enkelt plocka ut specifika delar av en sträng och använda dem för dina egna ändamål.

## Hur man gör det

Det finns två olika sätt att extrahera substrängar i Elixir. Det första sättet är att använda sig av funktionen `String.slice/3` som tar emot tre argument: den befintliga strängen, startindexet och slutindexet. Här är ett enkelt exempel på hur du skulle kunna använda denna funktion:

```Elixir
iex> String.slice("Hej världen!", 4, 10)
"världen"
```

Som du kan se så returnerar funktionen en ny sträng som består av tecken från den ursprungliga strängen från och med startindexet till och med slutindexet.

Det andra sättet att extrahera substrängar är genom att använda `String.slice/2` som bara tar emot två argument: den befintliga strängen och startindexet. I detta fall kommer funktionen att returnera alla tecken från startindexet till slutet av strängen.

```Elixir
iex> String.slice("Jag älskar Elixir!", 9)
"Elixir!"
```

## Djupdykning

En viktig sak att hålla i åtanke när du extraherar substrängar är vilket indexsystem som Elixir använder sig av. I Elixir börjar indexeringen av strängar alltid från 0 istället för 1. Detta betyder att i den första koden vi visade skulle "H" i "Hej världen" ha index 0, "e" skulle ha index 1 och så vidare.

Det är också värt att notera att båda funktionerna returnerar en ny sträng, vilket innebär att den ursprungliga strängen inte förändras. Detta är viktigt att komma ihåg om du vill använda substrängen för att modifiera den befintliga strängen.

## Se också

- [Elixir String-dokumentation](https://hexdocs.pm/elixir/String.html)
- [Elixir String-moduler](https://elixir-lang.org/docs/master/String.html#content)
- [Elixir String manipulation tutorials](https://elixirschool.com/en/lessons/basics/string/)