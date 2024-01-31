---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Kirjamerkkijonon suurentaminen tarkoittaa tekstin ensimmäisen kirjaimen muuttamista suureksi. Ohjelmoijat käyttävät toimintoa esimerkiksi käyttäjän nimen muotoiluun tai lauseiden yhdenmukaistamiseen.

## Kuinka:
```elixir
defmodule StringHelpers do
  def capitalize_string(str) when is_binary(str) do
    String.capitalize(str)
  end
end

IO.puts StringHelpers.capitalize_string("moi, maailma!") # => "Moi, maailma!"
```

## Syväsukellus
Kirjamerkkijonon suurentaminen on ollut osa ohjelmistokehitystä alusta lähtien, jolloin tekstinpätkien oikeaoppista esittämistä pidettiin tärkeänä. Elixir mahdollistaa merkkijonojen suurentamisen `String.capitalize/1`-funktiolla. Vaihtoehtoisesti voidaan käyttää `Regex`-kirjastoa, mutta se ei ole useimmiten tarpeen. `String.capitalize/1` komennossa suurennetuksi tulee aina ensimmäinen kirjain eikä esimerkiksi joka sana (kuten joissakin kielissä tehdään).

## Katso Myös
- Elixirin virallinen dokumentaatio String-moduulista: https://hexdocs.pm/elixir/String.html
- Unicode standardi ja suur-/pienkirjaimet: http://www.unicode.org/reports/tr21/tr21-5.html
