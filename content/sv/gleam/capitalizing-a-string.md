---
title:                "Gör om en sträng till versaler"
html_title:           "Gleam: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en string innebär att omvandla alla bokstäver i strängen till deras motsvarande versaler. Programmerare gör detta för att förbättra läsbarheten och för att skilja viktig text från mindre betydande information.

## Så här gör du:
Här är hur du kapitaliserar en sträng i Gleam:

```Gleam
import gleam/string

fn capitalise_string(given_string: String) -> String {
  string.capitalise(given_string)
}

let capitalised_string = capitalise_string("Hej, världen!")
```
När du kör koden blir output:

```Gleam
"Hej, Världen!"
```

## Djupdykning
Historiskt sett har text som skrivits i versaler använts för att signalera betydelse eller vikt. Det gäller också i kod: det kan hjälpa oss att lätt identifiera titlar, rubriker eller annan viktig text.

I Gleam, kan `string.capitalize/1` användas för att göra om en sträng till en sträng med enbart versaler. Denna funktion är enklare och mindre ansträngande än att manuellt hantera varje tecken ett och ett.

Detta är inte den enda metoden för att lösa uppgiften. Nackdelen med denna metod är att den inte hanterar accenterad text korrekt. För dessa fall kanske du vill använda en anpassad kapitaliseringsfunktion istället.

Kapitalisering utförs med UTF-8 teckenkodning, som ger stöd för nästan alla skrivna språk. Det innebär att den fungerar bra med internationell text.

## Se även
Se Gleams officiella dokumentation för mer information om [`string.capitalise`](https://hexdocs.pm/gleam_stdlib/gleam/string/#capitalise).
Kolla in denna artikel för att lära dig mer om [UTF-8](https://en.wikipedia.org/wiki/UTF-8).