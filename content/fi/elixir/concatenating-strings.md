---
title:                "Elixir: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Elixir on suosittu ohjelmointikieli, joka tarjoaa monipuolisia ominaisuuksia ohjelmistokehittäjille. Yksi näistä ominaisuuksista on mahdollisuus yhdistää merkkijonoja (strings). Tässä blogikirjoituksessa tarkastelemme tarkemmin, miksi ja miten voit käyttää tätä toimintoa Elixirissä.

## Miten

Merkkijonojen yhdistäminen eli concatenating on yksinkertaista Elixirissä. Voit käyttää siihen operaattoreita tai Elixirin funktioita, kuten "<>", "++" tai "String.concat()". Tässä koodiesimerkki ja tulostuskäsky:

```Elixir
concatenated_string = "Hello " <> "world!"
IO.puts concatenated_string
```

Tuloste: "Hello world!"

Voit myös yhdistää merkkijonoja muuttujien kanssa käyttämällä interpolointia, joka tekee koodista selkeämpää ja helpommin muokattavaa:

```Elixir
name = "John"
age = 25
greeting = "Hello, #{name}. You are #{age} years old."
IO.puts greeting
```

Tuloste: "Hello, John. You are 25 years old."

## Syvällisempi tarkastelu

Elixirissä merkkijonojen yhdistäminen on tehokasta ja nopeaa. Tämä johtuu siitä, että merkkijono on itse asiassa binääriarvo, jota voidaan käsitellä tehokkaasti Elixirin binääriarvojen käsittelyfunktioilla.

Lisäksi Elixirissä on myös mahdollista yhdistää suuria määriä merkkijonoja tehokkaasti käyttämällä stream-moduulia, joka käsittelee käsittelemättömiä merkkijonoja pieninä osina, vähentäen siten muistin tarvetta.

## Katso myös

- [Elixirin virallinen dokumentaatio merkkijonojen käsittelystä](https://hexdocs.pm/elixir/String.html)
- [Miten käsitellä merkkijonoja tehokkaasti Elixirissä?](https://medium.com/@gorails/how-to-efficiently-work-with-strings-in-elixir-31cc8efa417d)
- [Yhdistä useita merkkijonoja Elixirissä stream-moduulia käyttäen](https://dev.to/ianshward/streaming-in-elixir-ii-merging-inventory-items-dd2)