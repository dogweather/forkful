---
date: 2024-01-20 17:47:04.577607-07:00
description: "S\xE5 h\xE4r g\xF6r du: Elixir anv\xE4nder `String.length/1` f\xF6r\
  \ att hitta l\xE4ngden p\xE5 en str\xE4ng. H\xE4r \xE4r ett exempel."
lastmod: '2024-03-13T22:44:37.557938-06:00'
model: gpt-4-1106-preview
summary: "Elixir anv\xE4nder `String.length/1` f\xF6r att hitta l\xE4ngden p\xE5 en\
  \ str\xE4ng."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

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
