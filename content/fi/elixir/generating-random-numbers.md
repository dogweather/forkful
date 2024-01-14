---
title:    "Elixir: Sattumanvaraisten numeroiden luominen"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Miksi
Miksi haluat generoida satunnaisia lukuja Elixir-ohjelmoinnilla? Satunnaiset numerot ovat hyödyllisiä monissa sovelluksissa, kuten pelin luomisessa tai testitiedon tuottamisessa.

## Kuinka
Koodausesimerkit ja esimerkkilähtöööt "```Elixir ... ```" koodiblokeissa.

```Elixir
# Generoi satunnainen kokonaisluku välillä 1-10
Enum.random(1..10) 
#=> 7

# Generoi 5 satunnaista nimeä
["Mika", "Jenna", "Liisa", "Aleksi", "Emilia"] |> Enum.random(5) 
#=> ["Mika", "Liisa", "Jenna", "Aleksi", "Emilia"]
```

## Syvällisempi sukellus
Satunnainen numeroiden generointi Elixirissä käyttää Erlangin rand-moduulia, joka perustuu Mersenne Twister -algoritmiin. Rand-moduuli tarjoaa monipuolisen valikoiman toimintoja, joilla voi generoida satunnaisia lukuja eri muodoissa. Näihin kuuluu esimerkiksi kokonaislukuja, liukulukuja ja merkkijonoja. Rand-moduulin lähtöarvoa voidaan myös asettaa uudelleen tarpeen mukaan jolloin luvut toistetaan samassa järjestyksessä.

## Katso myös
* [Elixirin virallinen dokumentaatio satunnaisista numeroista](https://hexdocs.pm/elixir/Kernel.html#random/1)
* [Erlangin rand-moduulin dokumentaatio](http://erlang.org/doc/man/rand.html)