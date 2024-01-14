---
title:    "Ruby: Merkkijonon isoittaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Miksi Päästä Jos-stringi

Miksi: Päästä Jos-stringi voi olla tärkeä ohjelmoinnissa esimerkiksi kun haluat muuttaa käyttäjän syöttämän tiedon oikeaan muotoon tai tehdä tiedon käsittelystä helpompaa.

## Miten tehdä se

Päästä Jos-stringin välityksellä voit muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi. Tämä voi tehdä käytännölliseksi esimerkiksi silloin kun haluat muuttaa käyttäjän nimen alkukirjaimen isoksi. Käytännössä tämä olisi tehtävissä kahdella eri tavalla.

```
# Ensimmäinen tapa

nimi = "emilia"
puts nimi.capitalize

# Toinen tapa

nimi = "emilia"
puts nimi[0] = nimi[0].upcase
```

## Syöte ja Tuloste

```
Syöte: nimi = "emilia" (Merkkijono)

Tuloste: Emilia (Merkkijono)
```

## Syvempi Sukellus

Päästä Jos-stringin avulla voit myös tehdä muita muutoksia merkkijonolle, kuten muuttaa kaikki kirjaimet isoksi tai pieneksi. Tämä voidaan tehdä käyttämällä .upcase tai .downcase -metodeja.

Voit myös muuttaa pelkästään ensimmäisen kirjaimen isoksi ja jättää loput kirjaimet alkuperäiseen muotoon .capitalize -metodilla.

## Katso myös

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby String Methods](https://www.rubyguides.com/ruby-tutorial/string-methods/)
- [Ruby String Manipulation](https://www.educative.io/blog/ruby-string-manipulation)