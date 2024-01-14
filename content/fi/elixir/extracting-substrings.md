---
title:                "Elixir: Alipalautteen eristäminen"
simple_title:         "Alipalautteen eristäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi käyttäisit alimerkkijonojen erottelua?

Alimerkkijonojen erottelu on tärkeä osa ohjelmointia, sillä se mahdollistaa tietyn osan tekstin erottamisen suuremmasta merkkijonosta. Tämä voi säästää aikaa ja parantaa koodin luettavuutta.

## Miten tehdä alimerkkijonojen erottelu?

```Elixir
quote = "Elämä on kuin laatikko suklaata"
substring = String.slice(quote, 6, 12)
IO.puts(substring)
```

Tämä koodinpätkä tulostaa "kuin laatikko", joka on eritelty alkuperäisestä lainauksesta. Käyttämällä `String.slice` -funktiota ja antamalla sille alkupisteen ja pituuden, voit helposti erottaa haluamasi alimerkkijonon. Voit myös käyttää muita `String` -kirjaston funktioita, kuten `String.split` tai `String.replace`, tehdäksesi tarkempia alimerkkijonojen erotteluja.

## Syvempi sukellus alimerkkijonojen erotteluun

Alimerkkijonojen erottelu ei rajoitu vain yksinkertaisiin merkkijonoihin. Voit myös käyttää sitä esimerkiksi listoille tai tupleille. Esimerkiksi, jos haluat erottaa tietyn merkkijonon listassa, voit käyttää `Enum.find` -funktiota löytääksesi sen ja sitten käyttää `String.slice` sitä vastaavan alueen erottamiseen.

## Katso myös

- [Elixir String -kirjasto](https://hexdocs.pm/elixir/String.html)
- [Elixir List -kirjasto](https://hexdocs.pm/elixir/List.html)
- [Elixir Enum -kirjasto](https://hexdocs.pm/elixir/Enum.html)