---
title:    "Elixir: Merkkijonojen yhdistäminen"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi käyttää merkkijonojen yhdistämistä Elixir-ohjelmoinnissa?

Merkkijonot ovat tärkeä osa ohjelmointia, sillä ne mahdollistavat tekstipohjaisten tietojen käsittelyä. Merkkijonojen yhdistäminen eli konkatenointi onkin hyödyllinen taito, jota tarvitaan usein Elixir-ohjelmoinnissa. Se mahdollistaa eri muuttujien ja osien yhdistämisen yhdeksi merkkijonoksi, jota voidaan käyttää esimerkiksi tulostukseen tai tallentamiseen. Seuraavassa osiossa kerromme, miten merkkijonojen yhdistämistä voi toteuttaa Elixirissä.

## Miten tehdä merkkijonojen yhdistäminen Elixirissä

Merkkijonojen yhdistäminen Elixirissä onnistuu käyttämällä operaattoria `<>` tai `++`. Ensimmäinen yhdistää kaksi merkkijonoa yhdeksi, kun taas jälkimmäinen yhdistää listan merkkijonoja yhdeksi merkkijonoksi. Lisäksi Elixirissä voi käyttää myös `interpolate` -funktiota, joka yhdistää merkkijonon ja muuttujan arvon.

Esimerkiksi:

```Elixir
nimi = "Matti"
tervehdys = "Hei " <> nimi
IO.puts tervehdys
```

Tulostaa:

```
Hei Matti
```

```Elixir
kaupungit = ["Helsinki", "Tampere", "Turku"]
kaupunkilista = "Suomen kaupungit: " ++ kaupungit
IO.puts kaupunkilista
```

Tulostaa:

```
Suomen kaupungit: [Helsinki, Tampere, Turku]
```

Yhdistämisen lisäksi Elixirissä on myös muita hyödyllisiä merkkijonojen käsittelytoimintoja, kuten `String.upcase` ja `String.downcase`, jotka muuttavat merkkijonon kirjaimet ylä- tai alakirjaimiksi.

## Syvällisempi sukellus merkkijonojen yhdistämiseen

Merkkijonojen yhdistämisen lisäksi Elixirissä on myös muita tapoja käsitellä merkkijonoja, kuten `String.replace` ja `String.split`. Tämä antaa enemmän mahdollisuuksia tehdä monimutkaisempia merkkijonojen käsittelyjä. Lisäksi Elixirissä voi käyttää myös `Sigil` -syntaksia, joka mahdollistaa merkkijonojen luomisen erikoismerkkien avulla.

## Katso myös

- Elixir merkkijonojen dokumentaatio: https://hexdocs.pm/elixir/String.html
- Elixir merkkijonojen operointi: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#strings
- Elixir Sigil-syntaksin opas: https://elixir-lang.org/getting-started/sigils.html