---
title:                "Elixir: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift för programmerare, och Elixir tillhandahåller ett kraftfullt verktyg för att hantera detta. Med sök- och ersättningsfunktionerna kan du snabbt och effektivt hitta och ändra specifika textmönster i dina program.

## Så här gör du

För att söka och ersätta text i Elixir, kan du använda funktionerna `String.replace` och `String.replace_trailing` som tar emot tre argument - den ursprungliga strängen, sökmönstret och ersättningssträngen. Här är ett exempel på hur du kan använda dem:

```Elixir
str = "Hej världen!"
String.replace(str, "världen", "Elixir") # => "Hej Elixir!"
String.replace_trailing(str, "!", "?") # => "Hej världen?"
```

Som du kan se i exemplet kan sökmönstret vara en enkel sträng eller ett reguljärt uttryck. Båda funktionerna returnerar den modifierade strängen.

## Djupdykning

Dessa två funktioner är bara en del av Elixirs sök- och ersättningsfunktioner. Det finns flera andra funktioner som till exempel `String.replace_first` och `String.replace_last` som låter dig ange om du vill ändra den första eller sista förekomsten av ett sökmönster. Dessutom kan du använda funktionen `String.replace_all` för att ersätta alla förekomster av ett sökmönster i en sträng.

Det är också värt att notera att Elixir har stöd för både byte- och Unicode-strängar, och sök- och ersättningsfunktionerna fungerar på båda typerna.

## Se också

- [Elixir documentation on string manipulation](https://hexdocs.pm/elixir/String.html)
- [RegExr - regex editor and tester](https://regexr.com/) (på engelska)