---
title:                "Användning av reguljära uttryck"
html_title:           "Elixir: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Om du är en programmerare som arbetar med Elixir, har du förmodligen hört talas om regular expressions. Dessa är kraftfulla verktyg för mönstermatchning och sökning i strängar. Genom att använda regular expressions kan du effektivt hitta och manipulera data i dina Elixir-program.

## Så här
För att använda regular expressions i Elixir, behöver du först ladda in Regex-modulen. Du kan göra detta genom att skriva följande kod i din Elixir-fil:

```elixir
require Regex
```

Nu är du klar att använda regular expressions i ditt program. För att hitta ett visst mönster i en sträng kan du använda funktionen `Regex.match/2`. Till exempel, om du har en sträng som innehåller en e-postadress kan du använda följande kod för att hitta den:

```elixir
Regex.match(~r/\w+@\w+\.\w+/, "kontakt@exemplexempel.se")
```

Detta kommer att returnera en `match` struct som innehåller information om den matchade delen av strängen, såsom position och värde. Du kan sedan använda denna information för att extrahera och manipulera data i din kod.

Det finns också andra funktioner för att arbeta med regular expressions, såsom `Regex.scan/2`, som hittar alla matcher i en sträng, och `Regex.replace/3`, som låter dig ersätta en match med en annan sträng.

## Deep Dive
Regular expressions är en mycket kraftfull mekanism, men det kan vara lite knepigt att lära sig och använda dem korrekt. Det finns många olika symboler och operatorer som kan användas för att bygga mönster, och det är viktigt att förstå hur dessa fungerar tillsammans.

För att lära dig mer om hur du använder regular expressions i Elixir, rekommenderar vi att du utforskar dokumentationen för Regex-modulen och övar på att bygga och matcha olika mönster.

## Se också
- [Elixir Regex Dokumentation](https://hexdocs.pm/elixir/Regex.html)
- [RegExr.com - online regular expression tester](https://regexr.com/)
- [Learn Regular Expressions in 20 Minutes](https://www.youtube.com/watch?v=rhzKDrUiJVk) (video)