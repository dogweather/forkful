---
title:                "Huvudbokstaverar en sträng"
html_title:           "Gleam: Huvudbokstaverar en sträng"
simple_title:         "Huvudbokstaverar en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att stava ord på rätt sätt är viktigt, men ibland kan man behöva kapitalisera en hel sträng av text. Detta kan vara för att få en mer estetiskt tilltalande utskrift, för att följa specifika instruktioner eller helt enkelt för att uppfylla personliga preferenser. Oavsett anledning, så kommer vi i denna artikel att lära oss hur man gör detta i Gleam.

## Hur man gör

Att kapitalisera en sträng i Gleam är en enkel process som bara kräver några rader kod. Det första vi behöver göra är att importera *String* modulen genom att skriva ```import String``` längst upp i vår fil. Sedan kan vi enkelt använda den inbyggda funktionen ```String.uppercase_first``` som tar en sträng som argument och returnerar en ny sträng med första bokstaven i versaler. Här är ett exempel:

```
import String

let name = "greta"
let capitalized_name = String.uppercase_first(name)

io.println(capitalized_name)
```

Outputen av detta program kommer att vara "Greta", med den första bokstaven kapitaliserad.

## Deep Dive

För att förstå mer om kapitalisering av strängar i Gleam, är det viktigt att förstå att detta görs med hjälp av Unicode-kodpunkter. Detta innebär att även tecken från andra språk, såsom åäö, kan kapitaliseras korrekt. Dessutom finns det även andra funktioner inom *String* modulen som kan vara användbara för manipulation av strängar, såsom ```String.to_upper``` och ```String.to_lower``` som gör hela strängen till versaler respektive gemener.

## Se även

Här är några länkar till andra resurser och dokumentation som kan vara användbara när du arbetar med strängar i Gleam:

- [Gleam Documentation](https://gleam.run/documentation/)
- [String Module Documentation](https://gleam.run/modules/string/)
- [Unicode Code Points](https://unicode.org/charts/)