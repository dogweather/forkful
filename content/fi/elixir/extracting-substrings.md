---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Substringien erottaminen tarkoittaa suurempien merkkijonojen pilkkomista pienempiin osiin. Ohjelmoijat tekevät tätä usein datan analysoinnin, käsittelyn, ja rakenteen muokkaamisen tarpeiden vuoksi.

## Kuinka se tehdään: 

```Elixir
alue = 1..3
iso_merkkijono = "Tämä on esimerkki"
pieni_merkkijono = String.slice(iso_merkkijono, alue)
IO.puts pieni_merkkijono
```

Edellä oleva koodinpätkä erottaa merkkijonosta 'iso_merkkijono' alimerkkijono 'mä', käyttäen 'String.slice'-funktiota ja aluetta 1..3. Koodi tulostaa 'mä'.

## Sukellus syvemmälle

Substringien erottaminen on vanha ohjelmointikonsepti, joka on ollut olemassa ohjelmointikielissä alkaen ensimmäisistä kielistä, kuten Fortranista ja COBOL:sta. Elixiriä varten tämä omaksutaan String.slice-funktion kautta.

Vaihtoehtoisesti, voit käyttää myös funktiota 'binary_part/3'. Se on alhaisemman tason API ja sillä voi olla suorituskykyetuja tietyissä tilanteissa.

```Elixir
iso_merkkijono = "Enemmän esimerkkejä"
pieni_merkkijono =
  :erlang.binary_part(iso_merkkijono, {1, byte_size(iso_merkkijono) - 1})
IO.puts pieni_merkkijono
```

Edellä mainitussa koodissa "Enemmän esimerkkejä" -merkkijonosta on poistettu ensimmäinen merkki 'E', jolloin lopputuloksena on "nemmän esimerkkejä".

## Katso myös:

1. [Elixir String.slice](https://hexdocs.pm/elixir/String.html#slice/2) dokumentaatio Elixirin virallisella sivustolla.
2. [Erlang binary_part](https://erlang.org/doc/man/erlang.html#binary_part-3) dokumentaatio Erlangin virallisella sivustolla (Elixir on kirjoitettu Erlangissa).
3. Opas [Elixirin merkkijonokäsittelyyn](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html) Elixir lang -sivustolla.