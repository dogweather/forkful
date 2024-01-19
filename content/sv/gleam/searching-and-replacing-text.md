---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text är en grundläggande operation där en viss textsträng "söks" och sedan byts ut mot en annan. Programmerare utför detta för att modellera, analysera eller transformera data effektivt.

## Hur gör man:

Här är ett exempel på hur du kan söka och ersätta text inom Gleam:

```Gleam
import gleam/int
import gleam/string.{replace}

pub fn main() {
  let text = "Hej, Världen!"
  let updated_text = string.replace(text, "Världen", "Gleam")
  assert updated_text == "Hej, Gleam!"
}
```

När du kör den här kodspoken, kommer utmatningen att bli `"Hej, Gleam!"`.

## Fördjupning

(1) Historiskt sett har sök-och-ersätt operationer varit centrala i textredigerare från tidiga UNIX-verktyg som `sed` till moderna programmeringsmiljöer.
(2) Du kan också använda regelbundna uttryck (eller "regex") i många språk för att göra mer avancerade textförändringar. Gleam har dock inte inbyggt stöd för regex i dagsläget.
(3) I Gleam ändras textsträngar genom att skapa en ny sträng, eftersom strängar i Gleam är "immutable" (de kan inte ändras när de har skapats).

## Se även

* [Gleam Docs](https://gleam.run/tour/)
* [String.replace documentation](https://hexdocs.pm/gleam_stdlib/gleam/string.html#replace/3)
* [Text Manipulation in Gleam](https://github.com/gleam-experiments/text)