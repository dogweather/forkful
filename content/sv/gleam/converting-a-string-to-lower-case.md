---
title:                "Gleam: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till gemener (lower case) är ofta en viktig del av programmering då det kan förenkla jämförelser och sökningar i text. Det är även en vanlig uppgift när man arbetar med användarinput, där man vill hantera olika variationer av bokstäver för att undvika problem med jämförelser.

## Så här gör du
För att konvertera en sträng till gemener i Gleam, kan du använda den inbyggda funktionen `String.to_lower()`:

```Gleam
let str = "HeLLo"
let lower_str = String.to_lower(str)
```
Output: `lower_str = "hello"`

Du kan också använda pattern matching för att enkelt hantera både gemener och versaler:

```Gleam
fn to_lower_case(str) {
  case str {
    string -> String.to_lower(str)
    _ -> str
  }
}
```
Output: `to_lower_case("Hello") = "hello"`

## Djupdykning
Att konvertera en sträng till gemener innebär att du ändrar alla versaler (stora bokstäver) till gemener (små bokstäver). I Unicode finns det dock vissa tecken som inte har en gemener, såsom vissa akutaccent (´). Detta kan leda till oväntade resultat när man använder inbyggda funktioner för att konvertera strängar till gemener. Det är därför viktigt att ha detta i åtanke när du arbetar med olika språk och teckenuppsättningar.

## Se även
- [Gleam Documentation: String module](https://gleam.run/modules/std/string#to_lower)
- [Unicode: Lowercase Mapping](https://www.unicode.org/charts/PDF/U0000.pdf)