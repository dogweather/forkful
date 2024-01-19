---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut felsökningsoutput innebär att man visar innebörden i variabler eller uttryck i en körande kod. Det hjälper programmerare att förstå vad deras program gör vid en specifik tidpunkt, vilket är avgörande för att hitta och fixa fel.

## Hur göra:
I Gleam gör du det genom `io.debug`-funktionen. Här är ett exempel:

```Gleam
import gleam/io

fn main() {
  let message = "Hello Gleam!"
  io.debug(message)
}
```
Kör den här koden och du kommer se `Hello Gleam!` utskriven till konsolen.

## Fördjupning
Utskrift av felsökningsoutput har sina rötter i de tidigaste dagarna för programmering, när ingen avancerad debugger fanns tillgänglig. Det finns alternativ till `io.debug` i Gleam, till exempel kan du använda `io.println` för utskrift, men `io.debug` är att föredra för felsökning eftersom den är tydligt märkt som tillfällig output som hjälper till med felsökning, inte som en del av programmets ordinarie output. Genom att använda `io.debug` visar du att denna utskrift är tillfälligt och bör tas bort när felet är åtgärdat.

## Se även
För mer djupgående information om felsökning i Gleam, kolla in den officiella dokumentationen: 
- Gleam Felsökningsguide: https://hexdocs.pm/gleam/gleam/io/#debug
- Gleam IO module docs: https://hexdocs.pm/gleam/gleam/io/
- Gleam: 'io.debug' vs 'io.println': https://stackoverflow.com/questions/tagged/gleam