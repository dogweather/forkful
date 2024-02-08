---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:47:04.577607-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken i den. Programmerare gör detta för validering, formatering eller andra beräkningar som kräver att veta storleken på indata.

## Så här gör du:
Elixir använder `String.length/1` för att hitta längden på en sträng. Här är ett exempel:

```elixir
sträng = "Hej, Sverige!"
längd = String.length(sträng)
IO.puts längd
```

Sample output:

```
13
```

## Fördjupning
Stränglängd går tillbaka till de första programmeringsspråken – det är grundläggande. I Elixir, som använder Unicode-teckenuppsättningen, räknar `String.length/1` graferna (bokstavsrepresentationer) och inte de råa bytesen. Det betyder att det hanterar olika språk och tecken korrekt. Alternativ? `byte_size/1` ger råa bytes men använd det inte för stränglängd om du bryr dig om rätt teckenetal.

## Se också
- Elixir's officiella dokumentation för `String.length/1`: https://hexdocs.pm/elixir/String.html#length/1
- Unicode-standarden, för förståelse av grafer: http://www.unicode.org/standard/standard.html
- Elixir School för mer om strängar och binärer i Elixir: https://elixirschool.com/en/lessons/basics/strings/
