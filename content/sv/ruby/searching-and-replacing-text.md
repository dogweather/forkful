---
title:                "Sökning och ersättning av text"
html_title:           "Ruby: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text är när du letar efter en viss sträng av tecken i en text och ersätter den med en annan sträng. Det är ett mycket användbart verktyg för programmerare eftersom det kan hjälpa till att snabbt ändra stora mängder kod eller text. Det kan hjälpa till att undvika att man manuellt måste gå igenom varje förekomst av en viss sträng och ändra den var för sig.

## Hur man gör:
För att söka och ersätta text i Ruby kan du använda metoden `gsub`. Här är ett enkelt exempel:

```Ruby
text = "Hej, mitt namn är Ruby"

puts text.gsub("Ruby", "Sven")
```

Output:
```
Hej, mitt namn är Sven
```

Du kan också använda reguljära uttryck för att söka och ersätta. Till exempel:

```Ruby
text = "1234 abcd påstående"

puts text.gsub(/\d+/, "nummer")
```

Output:
```
nummer abcd påstående
```

## Djupdykning:
Sökning och ersättning är en funktion som finns i många programmeringsspråk och redigeringsverktyg. Det används ofta för att göra stora kodändringar snabbt och effektivt. Det finns många olika sätt att söka och ersätta text, inklusive reguljära uttryck och andra metoder som inte är specifika för teckenserie.

## Se även:
Läs mer om reguljära uttryck i Ruby [här](https://www.rubyguides.com/2015/06/ruby-regular-expressions/).