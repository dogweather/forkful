---
title:    "Elixir: Konvertera en sträng till gemener"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener, eller "lower case", kan vara användbart i många olika situationer. Det kan hjälpa till att jämföra strängar, söka igenom data eller förbättra läsbarheten i koden. Det är en enkel men användbar funktion som kan vara till stor hjälp för utvecklare.

## Hur man gör

För att konvertera en sträng till gemener i Elixir, kan du använda funktionen `String.downcase/1`. Detta tar en sträng som argument och returnerar en kopia av strängen där alla bokstäver är konverterade till gemener.

Här är en enkel kodexempel:

```elixir
original_sträng = "HEJ, DET HÄR ÄR EN STRÄNG"
gemensam_sträng = String.downcase(original_sträng)
IO.puts gemensam_sträng

# Output: hej, det här är en sträng
```

Som du kan se har strängen "HEJ, DET HÄR ÄR EN STRÄNG" konverterats till "hej, det här är en sträng". Det är viktigt att komma ihåg att funktionen `String.downcase/1` returnerar en kopia av strängen, så det ursprungliga värdet kommer fortfarande att vara oförändrat.

Du kan också konvertera en sträng till gemener med hjälp av funktionen `String.downcase/2`. Den här funktionen tar två argument - strängen som ska konverteras och språkoptionen. Språkoptionen används för att hantera bokstäver med accent, så att de också konverteras till gemener korrekt.

Här är ett exempel på hur du kan använda `String.downcase/2` med språkoptionen "sv" (svenska):

```elixir
gemensam_sträng = String.downcase("JAG ÄLSKAR ELIXIR!", "sv")
IO.puts gemensam_sträng

# Output: jag älskar elixir!
```

## Djupdykning

Konvertering av strängar till gemener är enkelt i Elixir tack vare den inbyggda funktionen `String.downcase/1`. Men vad händer egentligen under huven? En vanlig missuppfattning är att denna funktion använder sig av ASCII-kod för att konvertera bokstäverna. Men i själva verket använder den sig av Unicode-teckenkodningar som stödjer mycket fler språk och tecken.

Det finns också andra funktioner som kan användas för att konvertera strängar till gemener, såsom `String.to_lower/1`, som använder sig av standardbiblioteket Unicode.Case för att utföra konverteringen. Det är viktigt att ha ett överseende över vilken funktion du använder och vilka språk de stödjer för att undvika eventuella problem med accentuerade bokstäver.

## Se även

- [Elixir String Module documentation](https://hexdocs.pm/elixir/String.html).
- [Unicode.Case documentation for Elixir](https://hexdocs.pm/unicode/Unicode.Case.html).