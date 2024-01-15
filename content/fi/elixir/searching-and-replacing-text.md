---
title:                "Etsiminen ja tekstin korvaaminen"
html_title:           "Elixir: Etsiminen ja tekstin korvaaminen"
simple_title:         "Etsiminen ja tekstin korvaaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi etsiä ja korvata tekstiä? Yksi syy voi olla tarve muuttaa samankaltaisia merkkijonoja toisiksi tai päivittää vanhaa koodia uudempaan syntaksiin.

## Miten

Elixirin `String.replace/3`-funktio mahdollistaa tekstin etsimisen ja korvaamisen yhdellä rivillä. Alla on esimerkkejä tämän funktion käytöstä.

```elixir
# Etsi ja korvaa yksi sana toisella
String.replace("Tervetuloa Elixiriin!", "Tervetuloa", "Hei")
# output => "Hei Elixiriin!"

# Etsi ja korvaa useita sanoja samalla kertaa
String.replace("Elixir on hieno kieli!", ~r/on hieno/, "on mahtava")
# output => "Elixir on mahtava kieli!"

# Vaihda vain ensimmäinen esiintymä
String.replace("1,2,3,4,1,2,3,4", "1", "one", global: false)
# output => "one,2,3,4,1,2,3,4"

# Vaihda kaikki esiintymät
String.replace("Kaikki eivät ole täydellisiä.", "e", "i", global: true)
# output => "Kaikki ivät ole täydellisiä."
```

## Syvempi sukellus

Elixirin `String.replace/3` hyödyntää taustalla `Regex.replace/4`-funktiota, joka sallii säännöllisten lausekkeiden käytön haun kohteena. Tämä antaa käyttäjälle enemmän vaihtoehtoja ja tarkkuutta etsintään. Lisäksi, `Regex`-moduuli tarjoaa muitakin hyödyllisiä funktioita tekstien käsittelyyn, kuten `Regex.match?/2` ja `Regex.split/2`.

See Also

- Elixirin virallinen dokumentaatio: https://elixir-lang.org/docs.html
- Etsi ja korvaa teksti Elixirillä: https://elixirschool.com/fi/lessons/basics/binary-pattern-matching/#search-and-replace