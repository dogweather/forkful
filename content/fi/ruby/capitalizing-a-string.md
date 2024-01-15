---
title:                "Stringin ensimmäisen kirjaimen suurennus"
html_title:           "Ruby: Stringin ensimmäisen kirjaimen suurennus"
simple_title:         "Stringin ensimmäisen kirjaimen suurennus"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa Ruby-ohjelma, jossa muutetaan merkkijonon ensimmäinen kirjain isoksi? Ensinnäkin, se on usein tarpeen tietojenkäsittelyssä ja data-analyysissä. Toiseksi, se on yksi perusohjelmointitekniikoista, joita jokaisen Ruby-ohjelmoijan tulisi hallita.

## Miten

Muuttaaksesi merkkijonon ensimmäisen kirjaimen isoksi, tarvitset vain yhden Ruby-metodin: `capitalize`. Tämä metodi muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja muut seuraavat kirjaimet pieniksi.

```Ruby
merkkijono = "tämä on esimerkki"
puts merkkijono.capitalize

# Output:
# Tämä on esimerkki
```

## Syvemmälle

Vaihtoehtoinen tapa muuttaa merkkijonon ensimmäinen kirjain isoksi on käyttää `[]` merkintää. Tämä antaa sinulle enemmän kontrollia siitä, miten merkkijono muuttuu.

```Ruby
merkkijono = "esimerkki"
puts merkkijono[0].upcase + merkkijono[1..-1]

# Output:
# Esimerkki
```

Sinänsä kapitalisointi on melko yksinkertainen ja suoraviivainen toimenpide. Kuitenkin, joskus voit haluta olla tarkempi siitä, mitkä kirjaimet muutetaan isoksi ja pieneksi. Tässä tapauksessa voit käyttää `capitalize!` metodia, joka muuttaa merkkijonon suoraan alkuperäisessä muuttujassa.

```Ruby
merkkijono = "Esimerkki TEKSTIÄ"
merkkijono.capitalize!

puts merkkijono

# Output:
# Esimerkki tekstiä
```

## Katso myös

- [Ruby String Documentation](https://ruby-doc.org/core-3.0.1/String.html)
- [Programming in Ruby](https://learnrubythehardway.org/book/)
- [Capitalization in Ruby](https://www.rubyguides.com/2016/06/ruby-string-capitalize/)