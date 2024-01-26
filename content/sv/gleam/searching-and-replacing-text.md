---
title:                "Sökning och ersättning av text"
date:                  2024-01-20T17:57:51.216764-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sökning och ersättning av text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text innebär att hitta specifika strängar i data och byta ut dem mot annan text. Programmerare gör detta för att uppdatera kod, korrigera fel eller bearbeta data.

## Hur man gör:
```Gleam
import gleam/string

pub fn search_and_replace(subject: String, from: String, to: String) -> String {
  string.replace(&subject, &from, &to)
}

fn main() {
  let text = "Hej och välkommen till Gleam!"
  let updated_text = search_and_replace(text, "Gleam", "Gleam 1.0")
  io.println(updated_text)
}
```
Resultat:
```
Hej och välkommen till Gleam 1.0!
```

## Fördjupning:
Sök och ersätt funktionalitet har varit grundläggande i textredigerare och programmeringsverktyg sedan tidiga datorer. Varianter inkluderar kommando-line verktyg som `sed` i Unix-baserade system. I moderna programmeringsspråk, som Gleam, används funktioner som `replace` för att uppnå samma resultat på ett mer läsbart och underhållbart sätt. Dessa funktioner hanterar strängar som oföränderliga data, vilket betyder att varje ersättningsoperation skapar en ny sträng istället för att ändra den ursprungliga.

## Se även:
- The `sed` Unix stream editor: [GNU sed documentation](https://www.gnu.org/software/sed/manual/sed.html)
