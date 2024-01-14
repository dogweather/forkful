---
title:    "Elixir: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa sinun täytyy muuttaa päivämäärä merkkijonoksi Elixir-ohjelmointikielen avulla. Tämä voi olla tarpeen esimerkiksi, jos haluat tallentaa päivämäärän tietokantaan tai näyttää sen käyttäjälle selkeässä muodossa. Onneksi Elixir tarjoaa helpon tavan muuttaa päivämäärä merkkijonoksi.

## Miten tehdä

Converting date into string in Elixir on yksinkertaista käyttämällä `DateTime.to_iso8601` -funktiota. Se ottaa DateTime-objektin parametrina ja palauttaa merkkijonon, joka vastaa ISO 8601 -standardia. Voit myös käyttää `DateTime.to_string` -funktiota, joka ottaa DateTime-objektin ja halutun päivämäärän tarkkuuden parametrina. Tässä on esimerkki:

```Elixir
DateTime.to_iso8601(~U[2020-12-31 18:30:00])
#=> "2020-12-31T18:30:00Z"

DateTime.to_string(~U[2020-12-31 18:30:00], :second)
#=> "2020-12-31 18:30:00"
```

## Syvällinen sukellus

Elixirillä on myös useita muita funktioita, jotka tarjoavat joustavuutta päivämäärän muuntamisessa merkkijonoksi. Voit esimerkiksi käyttää `DateTime.to_naive` -funktiota, joka palauttaa DateTime-objektin ilman aikavyöhykettä. Voit myös käyttää `DateTime.add` -funktiota lisätäksesi tai vähentääksesi aikaa DateTime-objektista. Lisätietoja voit lukea Elixirin virallisesta dokumentaatiosta.

## Katso myös

- [Elixirin virallinen dokumentaatio](https://hexdocs.pm/elixir/DateTime.html)
- [Elixirin date- ja time-moduulit](https://elixirschool.com/en/lessons/advanced/date-time/)
- [Elixirin päivämäärä- ja aikavyöhykemoduulien käyttö](https://thoughtbot.com/blog/working-with-elixir-and-datetimes)