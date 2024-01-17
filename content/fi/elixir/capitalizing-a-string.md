---
title:                "Merkitsevä nauha"
html_title:           "Elixir: Merkitsevä nauha"
simple_title:         "Merkitsevä nauha"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Kun puhutaan merkkijonojen muokkaamisesta Elixir-ohjelmoinnissa, monille tulee ensimmäisenä mieleen "stringin" muuttaminen suurilla kirjaimilla. Tämä tarkoittaa yksinkertaisesti sitä, että kaikki merkkijonon kirjaimet vaihdetaan isoiksi kirjaimiksi. Tämä muutos voidaan tehdä eri syistä, kuten tietojen yhtenäistämiseksi tai tietojen vertailun helpottamiseksi.

## Kuinka tehdä?

Elixirissa merkkijonojen muuttaminen isoiksi kirjaimiksi voidaan tehdä käyttämällä funktiota nimeltä `String.upcase`. Tämä funktio ottaa parametrina merkkijonon ja palauttaa uuden merkkijonon, jossa kaikki kirjaimet on muutettu isoiksi kirjaimiksi.

```Elixir
String.upcase("Hei maailma") # => "HEI MAAILMA"
```

## Syvemmälle sukeltaminen

On huomioitava, että Elixirissa merkkijonot ovat immuutabeleja eli muuttumattomia. Tämä tarkoittaa, että `String.upcase`-funktio ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden muokatun merkkijonon. Lisäksi merkkijonojen muuttaminen isoiksi kirjaimiksi voi vaikuttaa suorituskykyyn, jos käytetään valtavia merkkijonoja. Tällöin kannattaa harkita muita muokkausmetodeja, kuten esimerkiksi merkkijonojen vertailua käyttämällä `String.downcase`-funktiota.

## Katso myös

- [String.upcase dokumentaatio](https://hexdocs.pm/elixir/String.html#upcase/1)
- [Elixir String-moduulin dokumentaatio](https://hexdocs.pm/elixir/String.html)