---
title:                "Merkkijonon alkukirjaimen suurentaminen"
html_title:           "Elixir: Merkkijonon alkukirjaimen suurentaminen"
simple_title:         "Merkkijonon alkukirjaimen suurentaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
"Stringin" kirjainten muuttaminen isoksi on ohjelmoinnin rutiini, jossa muutetaan merkkijonon jokainen merkki isoiksi kirjaimiksi. Ohjelmoijat tekevät tämän esimerkiksi parantaakseen luettavuutta tai vertailun yhtenäistämiseksi, koska kirjainkoko mielletään usein semanttiseksi eikä sisällölliseksi eroksi.

## Näin teet:
Voit "capitalisoida" merkkijonon Elixirissä käyttämällä `String.upcase/1` -toimintoa.

```Elixir
io |> format("~s\n", [String.upcase("hello world")])
```

Koodin palautusarvo on:

```Elixir
"HELLO WORLD"
```

## Syvällisemmin:
Historiassa merkkijonot esitettiin aluksi vain isoilla kirjaimilla, koska varhaiset tietokonejärjestelmät eivät tukeneet pieniä kirjaimia. Nykyisin sekä isojen että pienten kirjainten tukeminen on standardi.

Merkkijonojen isojen kirjainten vaihtoehtona voidaan käyttää funktiota `String.capitalize/1`, joka muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja muiden kirjainten pieniksi.

```Elixir
String.capitalize("hello world")
```

Koodin lopputulos on "Hello world".

Toteutuksen yksityiskohdista tärkeä seikka on, että `String.upcase/1` -toiminnolla on Unicode-tuki, joten se toimii oikein myös esimerkiksi ääkkösten kanssa.

## Katso myös:
Lisää tietoa Elixirin merkkijonoista löytyy [virallisesta dokumentaatiosta](https://hexdocs.pm/elixir/String.html). Älä unohda tutustua myös muita hyödyllisiä funktioita, kuten `String.downcase/1` ja `String.capitalize/1`.