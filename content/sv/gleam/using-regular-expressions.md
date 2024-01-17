---
title:                "Användning av reguljära uttryck"
html_title:           "Gleam: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Användningen av regelbundna uttryck är en vanlig och användbar teknik inom programmering. Det är ett sätt för utvecklare att söka och manipulera textsträngar baserat på ett specifikt mönster. Detta gör det möjligt att snabbt och effektivt utföra uppgifter som jämförelse, ersättning och extrahering av data. Reguljära uttryck används ofta för att hantera data inom textbaserade filer eller för att validera inmatning från användare.

## Hur man använder:
Att använda reguljära uttryck i Gleam är enkelt och kräver bara några rader med kod. För att använda dem, inkludera "gleam/regexp" biblioteket och ange sedan det specifika mönstret som du vill matcha i ett reguljärt uttryck. Här är ett exempel på enkel användning:

```Gleam
import gleam/regexp

let email_regex = regexp.regex("^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[_a-z0-9-]+(\.[_a-z0-9-]+)*(\.[a-z]{2,})$")
let email = "example@email.com"

regexp.match(email_regex, email) // Returns `true`
```

I detta exempel definierar vi ett reguljärt uttryck som matchar e-postadresser och kontrollerar om e-postadressen "example@email.com" matchar det mönstret.

## Djupdykning:
Reguljära uttryck har funnits sedan 1950-talet och har sedan dess utvecklats och implementerats i flera olika programmeringsspråk. Alternativ till reguljära uttryck inkluderar strängmanipulering med inbyggda funktioner eller att använda andra strukturtyper som listor eller träd. Det bör noteras att användningen av reguljära uttryck ibland kan vara komplicerad och svår att läsa, vilket kan leda till buggar eller förvirring, så det är viktigt att vara försiktig och testa noggrant.

## Se även:
- [Gleam's officiella dokumentation om reguljära uttryck](https://gleam.run/documentation/drafts/regexp.html)
- [En guide för användning av reguljära uttryck i andra programmeringsspråk](https://www.linode.com/docs/development/regular-expressions-overview/)