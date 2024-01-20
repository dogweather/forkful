---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Vad och Varför?

Att läsa kommandoradsargument är att fånga data som användaren ger vid körningen av ett program. Programmerare gör detta för att tillåta viss personalisering och flödeskontroll i programmet.

# Hur gör man:

Här är ett enkelt exempel på Gleam-kod som visar hur du läser kommandoradsargument:

```Gleam
import gleam/list.{from_list, map, to_list}
import gleam/option.{unwrap}
import gleam/string.{parse_int, to_string}

fn main(args: List(String)) {
  let args = from_list(args)
  |> map(parse_int)
  |> to_list
  |> map(unwrap)
  |> unwrap
  |> to_string
  |> io.println
}

pub fn start() {
  main(env.args())
}
```

Om användaren kör programmet med `gleam run . 12 24 36`, kommer outputten vara `["12","24","36"]`.

# Djupdykning

Historiskt sett blev konceptet med kommandoradsargument användbart för att styra beteendet hos UNIX-verktyg. Det är än idag en populär teknik för att göra ett program mer flexibelt.

Ett alternativ till att läsa kommandoradsargument är att använda konfigurationsfiler. Men detta kan vara överkill för små program eller skript.

När du arbetar med kommandoradsargument i Gleam översätts parametrarna till en Gleam-lista. Denna lista kan sedan manipuleras med Gleam-funktioner som map, foldl etc. för att uppnå önskade resultat.

# Se också

För att dyka djupare in i Gleam-programmering och kommandoradsargument, konsultera följande resurser:

1. [Gleam's stora bok om programmering](https://gleam.run/book/)
2. [Erlang -- User's Guide](https://erlang.org/doc/man/erl.html)
3. [Elixir School - Kommandoradsargument](https://elixirschool.com/en/lessons/advanced/escripts/)