---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "Elixir: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Satunnaislukujen generoiminen on prosessi, jossa tietokone luo numeroita sattumanvaraisesti. Tämä on tärkeä osa ohjelmointia monella eri tavalla - se voi olla hyödyllistä esimerkiksi arpajaisten tai pelien luomisessa tai testidatan tuottamisessa.

## Miten:
Esimerkkejä ohjelmoinnista sekä näytöskuvia:
```
Elixir Enum.shuffle([1, 2, 3]) # => [2, 3, 1]
```

```
Elixir :random.uniform() # => 0.47548163296987905
```

```
Elixir Math.pow(2,3) # => 8
```

## Syväsukellus:
Satunnaislukujen generoiminen on ollut tärkeä osa tietokoneiden historiaa jo pitkään, ja siihen liittyy monia mielenkiintoisia algoritmeja ja tekniikoita. Toimintaympäristöjen lisäksi on myös muita tapoja generoida satunnaislukuja - esimerkiksi käyttämällä ulkoisia laitteita, kuten hiirtä tai satelliittisignaaleja.

## Katso myös:
- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/Kernel.html#random-number-generation)
- [Satunnaislukujen generoiminen pelien koodauksessa](https://www.gamasutra.com/blogs/HermanTulleken/20161014/282737/Random_number_generators_in_game_design.php)
- [Random data generation with Elixir](https://pragmaticstudio.com/tutorials/random-data-generation-with-elixir)