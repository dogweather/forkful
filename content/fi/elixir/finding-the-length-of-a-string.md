---
title:                "Elixir: Merkkijonon pituuden löytäminen"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Stringin pituuden laskeminen on tärkeä taito Elixir-ohjelmoinnissa, sillä se mahdollistaa merkkijonojen käsittelyn ja muokkaamisen. Tämä taito on erityisen hyödyllinen, kun halutaan esimerkiksi tarkistaa, että syötetyt tiedot ovat oikean mittaisia tai leikata merkkijonoja halutun pituisiksi. Seuraavaksi kerron, miten voit helposti löytää stringin pituuden Elixirillä.

## Kuinka?

Stringin pituuden laskeminen onnistuu Elixirillä käyttämällä sisäänrakennettua funktiota `length()`. Tämä funktio ottaa parametrinaan merkkijonon ja palauttaa sen pituuden. Alla on esimerkki koodista, jossa lasketaan stringin "Hei, maailma!" pituus ja tulostetaan se konsoliin.

```Elixir
string = "Hei, maailma!"
pituus = length(string)
IO.puts "Stringin pituus on #{pituus}."
```

Yllä olevan koodin tuloste on `Stringin pituus on 13.` Huomaa, että myös välilyönnit lasketaan mukaan merkkeihin, joten ne vaikuttavat myös stringin pituuteen.

Voit myös käyttää `length()`-funktiota yhdessä muiden merkkijonojen käsittelyä helpottavien funktioiden kanssa. Esimerkiksi voit käyttää `length()`-funktiota yhdessä `String.upcase()`-funktion kanssa, jolloin saat selville isolla kirjoitetun merkkijonon pituuden.

```Elixir
string = "Tämä on tärkeä taito"
isoksi = string |> String.upcase()
pituus = length(isoksi)
IO.puts "Isolla kirjoitetun stringin pituus on #{pituus}."
```

Tämän koodin tuloste on `Isolla kirjoitetun stringin pituus on 22.`

## Syvemmälle

Elixirissä merkkijonot ovat listoja, jotka sisältävät merkkejä. Tämän vuoksi `length()`-funktio käsittelee merkkijonoja samalla tavalla kuin muitakin listoja. Voit esimerkiksi käyttää `Enum.map()`-funktiota muuttamaan merkkijonon jokainen merkki numeroksi ja laskea sen jälkeen `length()`-funktiolla listan pituuden. Esimerkiksi alla olevalla koodilla saat selville, monellako eri merkillä merkkijono "Tässä on kolme erilaista kirjainta." on.

```Elixir
string = "Tässä on kolme erilaista kirjainta."
numerot = string |> String.upcase() |> String.graphemes() |> Enum.map(&(&1 |> String.to_integer()))
pituus = length(numerot)
IO.puts "Merkeillä #{string} on #{pituus} erilaista kirjainta."
```

Tämän koodin tuloste on `Merkeillä Tässä on kolme erilaista kirjainta. on 19 erilaista kirjainta.`

## Katso myös

- Elixirin String-moduuli: https://hexdocs.pm/elixir/String.html
- `Enum.map()`-funktion dokumentaatio: https://hexdocs.pm/elixir/Enum.html#map/2