---
date: 2024-01-20 17:57:31.203811-07:00
description: "How to: (Kuinka tehd\xE4:) Elixiriss\xE4 tekstin etsiminen ja korvaaminen\
  \ onnistuu `String`-moduulin avulla. Alapuolella pari esimerkki\xE4."
lastmod: '2024-04-05T21:53:57.768355-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) Elixiriss\xE4 tekstin etsiminen ja korvaaminen onnistuu\
  \ `String`-moduulin avulla."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## How to: (Kuinka tehdä:)
Elixirissä tekstin etsiminen ja korvaaminen onnistuu `String`-moduulin avulla. Alapuolella pari esimerkkiä:

```elixir
# Tekstin korvaaminen toisella
original_text = "Hello World!"
new_text = String.replace(original_text, "World", "Elixir")
IO.puts(new_text)
# Output: Hello Elixir!

# Regexin avulla voi tehdä monimutkaisempia korvauksia
regex = ~r/world/i
fixed_text = String.replace(original_text, regex, "Elixir")
IO.puts(fixed_text)
# Output: Hello Elixir!
```
Huomaa, että regex-esimerkissä käytimme `i`-lipua, joka tekee hausta kirjainkoosta riippumattoman.

## Deep Dive (Syväsukellus)
Searching and replacing text in Elixir leverages the power of Erlang's pattern matching and binary processing capabilities. Pattern matching on binaries is efficient and fast, which is why such tasks are generally easy on the system resources.

Ennen Elixirin ja Erlangin aikaa, tekstien käsittelyyn käytettiin muita kieliä kuten Perl, joka oli tunnettu tehokkaasta tekstinkäsittelystään. Elixirin regex-ominaisuuksia voitetaan `:re`-moduulia Erlangista, mikä mahdollistaa monipuoliset tekstinkäsittelytoiminnot.

On olemassa myös muita kirjastoja, kuten `Stringex`, jotka tarjoavat lisätoimintoja tekstin käsittelyyn. Tämä saattaa olla hyödyllistä, kun etsit ja korvaat monimutkaisempia merkkijonomalleja.

Itse tekstinkorvauksen takana on erilaisia algoritmeja. Yksinkertaisimmissa tapauksissa voidaan käyttää suoria merkkijonohakuja, mutta monimutkaisemmissa, kuten regex-hauissa, käytetään koneellista tilaa hyväksikäyttäviä algoritmeja.

## See Also (Katso Myös)
- Elixirin virallinen dokumentaatio: [String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4) ja [Regex-moduuli](https://hexdocs.pm/elixir/Regex.html)
- [Learn Elixir](https://elixir-lang.org/learning.html) - lisämateriaalia Elixirin oppimiseen
- [Erlangin :re-moduuli](http://erlang.org/doc/man/re.html) - syvemmälle regex-ominaisuuksiin Erlangissa
- [Stringex-kirjaston GitHub-sivu](https://github.com/rsl/stringex) - laajennettujen tekstinkäsittelytoimintojen kirjasto
