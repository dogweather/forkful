---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Säännölliset lausekkeet ovat mallipohjaisia hakuja tekstissä. Niitä käytetään monimutkaisten merkkijonohakujen ja -korvausten toteuttamiseen nopeasti ja tehokkaasti.

## How to:
Elixirisssa säännöllisiä lausekkeita käytetään `Regex`-moduulin avulla:

```elixir
# Haku mallilla
regex = ~r/hello/
"hello world" |> Regex.match?(regex)
# Output: true

# Korvaus
"hello world" |> Regex.replace(~r/o/, "a")
# Output: "hella world"

# Kaikki osumat listana
"hello world, hello universe" |> Regex.scan(~r/hello/)
# Output: [["hello"], ["hello"]]
```

## Deep Dive
Säännölliset lausekkeet juontavat juurensa 1950-luvun lopun automaattiteoriaan. Elixir käyttää BEAM-koneen (Erlang virtuaalikone) tarjoamaa toteutusta, mikä on yhteneväinen Perl-tyylisiin ilmaisuihin. Vaihtoehtoina ovat esimerkiksi merkkijonojen sisäänrakennetut funktiot ja String-moduuli, mutta ne eivät ole yhtä joustavia monimutkaisissa hauissa.

## See Also
Elixirin virallinen dokumentaatio: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
Säännöllisten lausekkeiden perusteet: [https://www.regular-expressions.info](https://www.regular-expressions.info)
BEAM: [https://erlang.org/doc/apps/erts/erl_intro.html](https://erlang.org/doc/apps/erts/erl_intro.html)
