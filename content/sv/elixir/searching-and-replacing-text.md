---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att söka och ersätta text innebär att identifiera alla instanser av en specifik sträng i din kod och byta ut dem mot en annan sträng. Det är ett vanligt verktyg i en programmerares verktygslåda för att hjälpa till att förändra, korrigera eller förbättra koden snabbt och effektivt.

## Så här:
Här är ett grundläggande exempel på hur du kan söka och ersätta text i Elixir:

```Elixir
defmodule StringOperations do
  def replace(old_string, to_replace, replace_with) do
      String.replace(old_string, to_replace, replace_with)
  end
end

IO.puts StringOperations.replace("Jag programmerar Elixir", "Elixir", "Python")
#=>
"Jag programmerar Python"
```
Koden söker igenom given sträng, identifierar ordet "Elixir" och byter ut det mot "Python".

## Djupdykning
Searching och replacing av text har djupa rötter i programmeringsspråkshistoria, med Unix-verktyget `sed` som en av de äldsta och mest kända exemplen.

Alternativt till `String.replace` finns funktionen `Regex.replace(regex, subject, replacement)` i Elixir som tillåter mer komplexa byten med reguljära uttryck.

Viktig detalj att notera är att Elixir är opåverkbar (immutable), vilket innebär att textsträngar inte modifieras på plats utan snarare returnerar nya strängar med de önskade ändringarna.

## Se också
1. [Elixir String-module documentation](https://hexdocs.pm/elixir/String.html)
2. [Elixir School String lesson](https://elixirschool.com/lessons/shell/history-and-help/)
3. [Elixir Regex-module documentation](https://hexdocs.pm/elixir/Regex.html)