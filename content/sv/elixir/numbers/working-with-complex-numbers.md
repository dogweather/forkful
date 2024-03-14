---
date: 2024-01-26 04:39:15.192483-07:00
description: "Komplexa tal har en reell del och en imagin\xE4r del (som `3 + 4i`).\
  \ De anv\xE4nds inom ingenj\xF6rsarbete, fysik och vissa dataproblem. Programmerare\
  \ arbetar med\u2026"
lastmod: '2024-03-13T22:44:37.560931-06:00'
model: gpt-4-0125-preview
summary: "Komplexa tal har en reell del och en imagin\xE4r del (som `3 + 4i`). De\
  \ anv\xE4nds inom ingenj\xF6rsarbete, fysik och vissa dataproblem. Programmerare\
  \ arbetar med\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal har en reell del och en imaginär del (som `3 + 4i`). De används inom ingenjörsarbete, fysik och vissa dataproblem. Programmerare arbetar med dem för simuleringar, signalbehandling och för att lösa vissa typer av matematiska problem effektivt.

## Hur man gör:
Elixir har inte inbyggda komplexa tal, så vi skapar våra egna eller använder ett bibliotek, som `ComplexNum`. Här är ett snabbt exempel med ett bibliotek:

```elixir
# Antag att du har installerat ComplexNum
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Skapa komplexa tal och addera dem
c1 = {3, 4}   # representerar 3 + 4i
c2 = {2, -3}  # representerar 2 - 3i
result = ComplexMath.add(c1, c2)
IO.puts "Resultatet är: #{inspect(result)}"
```

Detta skulle ge utskriften:
```
Resultatet är: {5, 1}
```

Det betyder att summan av `3 + 4i` och `2 - 3i` är `5 + 1i`.

## Djupdykning
Komplexa tal dök upp i historien eftersom vanliga gamla tal inte kunde hantera kvadratrötter av negativa tal. Det var inte förrän på 1600-talet som de togs på allvar, tack vare matematiker som René Descartes och Gerolamo Cardano.

I Elixir använder du ofta tupler som `{3, 4}` för komplexa tal, eller använder ett dedikerat bibliotek för att undvika att uppfinna hjulet på nytt. Bibliotek är vanligtvis bättre - de hanterar det knepiga som multiplikation och division, som blir komplicerade på grund av den imaginära enheten 'i' (FYI: `i` upphöjt till två är lika med `-1`).

## Se också
Kolla in dessa resurser:
- [ComplexNum Bibliotek](https://hex.pm/packages/complex_num) för Elixirs pakethanterare, Hex.
- [Elixir School](https://elixirschool.com/en/), för avancerade Elixir-ämnen och övningar.
- [Erlang -- math Module](http://erlang.org/doc/man/math.html), som Elixir använder under huven, för andra matematiska behov.
