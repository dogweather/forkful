---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att söka och ersätta text innebär att leta efter specifika strängar i en text och byta ut dem mot något annat. Programmerare gör detta för att snabbt uppdatera data, korrigera fel eller transformera information.

## Så här gör du:
I Elm kan du använda funktionerna `String.contains`, `String.replace` och `String.split` för att söka och ersätta text. Här är ett enkelt exempel:

```Elm
import String

main =
  let
    text = "Hej, världen!"
    newText = String.replace "världen" "Sverige" text
  in
    newText
```
Kör detta kod och du kommer se att "Hej, Sverige!" skrivs ut.

## Djupdykning
1. Historisk bakgrund: Funktioner för att söka och ersätta text har varit kärnfunktioner i programmeringsspråk sedan tidigt 70-tal. De har sitt ursprung i behovet av att manipulera stora textmängder, som databaser eller textdokument.
2. Alternativ: Du kan även använda bibliotek som `regex-elm` för att göra mer komplexa sökningar och ersättningar baserat på reguljära uttryck.
3. Implementationsdetaljer: I Elm, `String.replace` tar tre argument: söksträngen, ersättningssträngen och den ursprungliga texten. Den använder interna funktioner för att först hitta alla förekomster av söksträngen, och byter sedan ut dem mot ersättningssträngen.

## Se också
Om du vill lära dig mer om hur man hanterar strängar och text i Elm, kan följande källor vara till hjälp:

- Elm's officiella dokumentation om `String` modulen: [Elm String docs](https://package.elm-lang.org/packages/elm/core/latest/String)
- GitHub repo för `regex-elm` biblioteket: [elm/regex](https://github.com/elm/regex)
- En bra introduktionsbok för Elm: [Elm in Action](https://www.manning.com/books/elm-in-action)