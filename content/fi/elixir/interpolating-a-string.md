---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Merkkijonojen interpolointi tarkoittaa muuttujien arvojen lisäämistä suoraan merkkijonoihin. Ohjelmoijat käyttävät sitä koodin lyhentämiseksi ja lukemisen helpottamiseksi.

## Kuinka toimii:

Käytä `"#{}"` syntaksia muuttujien lisäämiseksi merkkijonoihin.

```elixir
name = "Jorma"
IO.puts "Hei, #{name}!"
```

Tuo tulostaa `Hei, Jorma!` konsoliin.

## Syvempi sukellus

Merkkijonojen interpolointi on ollut ohjelmoinnissa jo pitkään, ja sitä käytetään lähes kaikissa moderneissa ohjelmointikielissä, Elixir mukaan lukien. Elixirissä interpolointi on toteutettu käyttämällä Erlangin binääri- ja murtolukukirjastoa, mikä mahdollistaa tehokkaan suorituskyvyn.

Vaihtoehtoina merkkijonojen interpoloinnille voit käyttää `<>` operaattoria tai `IO.inspect` funktiota. Kuitenkin `"#{}"` on suositeltavaa, koska se on suoraviivaisempi ja selkeämpi.

## Katso myös

1. Elixir-lang sivusto stringien käsittelystä: https://elixir-lang.org/getting-started/basic-types.html#strings
2. Elixir School merkkijonojen interpoloinnista: https://elixirschool.com/en/lessons/basics/strings/