---
title:                "Elixir: Merkkijonon ensimmäisten kirjainten isointaminen"
simple_title:         "Merkkijonon ensimmäisten kirjainten isointaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat pystyä muuttamaan merkkijonon alkukirjaimen isoksi. Tämä voi olla hyödyllistä esimerkiksi, kun esität käyttäjille nimiä tai otsikoita kauniimmassa muodossa.

## Miten tehdä

Onneksi Elixirissä on helppo tapa tehdä tämä käyttämällä String.capitalize funktiota. Tämä funktio ottaa merkkijonon sisään ja palauttaa uuden merkkijonon, jossa ensimmäinen kirjain on iso.

```Elixir
nimi = "mikko"
nimi = String.capitalize(nimi)

IO.puts nimi

# Output: Mikko
```

## Syvällinen syventyminen

On myös mahdollista antaa ylimääräinen parametri String.capitalize funktiolle, joka määrää, halutaanko muuttaa myös loput kirjaimet pieniksi. Oletusarvoisesti tämä on "true", joten voit jättää tämän pois, jos haluat vain muuttaa ensimmäisen kirjaimen isoksi.

```Elixir
teksti = "tÄmÄ ON eRiKOInen laUse"
teksti = String.capitalize(teksti, true)

IO.puts teksti

# Output: Tämä on erikoinen lause
```

## Katso myös
- [Elixirin virallinen dokumentaatio String.capitalizeista](https://hexdocs.pm/elixir/String.html#capitalize/2)
- [Elixirin opetusohjelma merkkijonon käsittelystä](https://elixir-lang.org/getting-started/typespecs-and-behaviours.html#string-manipulation)