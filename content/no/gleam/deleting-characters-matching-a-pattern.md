---
title:                "Slette tegn som matcher et mønster"
date:                  2024-01-20T17:42:14.478734-07:00
model:                 gpt-4-1106-preview
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
I programmering betyr det å slette tegn som matcher et mønster å filtrere ut bestemte tegn fra en streng basert på kriterier, som en del av datarensing eller inputvalidering. Vi gjør det for å kontrollere data før bruk eller lagring for å unngå feil og sikre konsistens.

## How to:
I Gleam kan vi bruke `string.replace` for å slette tegn som matcher et mønster. La oss se på et eksempel:

```gleam
import gleam/string

fn delete_pattern(text: String, pattern: String) -> String {
  string.replace(text, pattern, "")
}

pub fn main() {
  let original_text = "Kaffe er best uten sukker!"
  let clean_text = delete_pattern(original_text, " sukker")
  println(clean_text) // "Kaffe er best uten!"
}
```

Koden over sletter ordet "sukker" fra tekststrengen.

## Deep Dive
Før regulære uttrykk ble utbredt, var teksthåndtering begrenset og man måtte ofte utvikle egne funksjoner for å manipulere strenger. Regulære uttrykk revolutionized how we manipulate texts, allowing complex patterns to be described and matched with simplicity.

I Gleam får man forskjellige alternativer til `string.replace`, som `string.trim` og `string.slice`, men disse er mer begrenset i bruk til henholdsvis fjerning av hvite tegn og sub-streng operasjoner.

Implementasjonen av tegnsletting i Gleam ivaretas av funksjoner i `gleam/string`-modulen, som igjen bygger på Erlang's string-håndteringsfunksjoner, siden Gleam kompilerer til Erlang bytekode.
