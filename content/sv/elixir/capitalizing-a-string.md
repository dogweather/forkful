---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng betyder att göra första bokstaven i varje ord stor. Programmerare gör detta för att standardisera textdata eller förbättra användargränssnittet.

## Hur gör man:
```elixir
defmodule StringExample do
  def capitalize_string(str) do
    str
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts StringExample.capitalize_string("hej elixir världen")
# Output: "Hej Elixir Världen"
```

## Djupdykning
Från början av datavetenskapen har textbehandling varit en kärna i programmering, med strängkapitalisering som en basal funktion i många språk. I Elixir utförs detta genom `String.capitalize/1` funktionen, som ändrar det första tecknet i varje ord till versal och de återstående till gemener. Alternativt kan man använda `String.upcase/1` för att göra alla bokstäver stora, men det skulle inte respektera kapitaliseringsregler. Det är viktigt att notera att `String.capitalize/1` också hanterar Unicode korrekt, vilket är vitalt i modern programmering där globalisering är nyckeln.

## Se även
- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Unicode in Elixir](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#unicode-and-code-points)
- [An introduction to Elixir's Enum module](https://hexdocs.pm/elixir/Enum.html)