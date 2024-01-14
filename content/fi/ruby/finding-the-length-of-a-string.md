---
title:                "Ruby: Säikeen pituuden löytäminen"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi koodaajat ovat niin kiinnostuneita merkkijonon pituuden laskemisesta? Yksinkertaisesti sanottuna, merkkijonon pituuden laskeminen on tärkeä taito, joka auttaa käsittelemään merkkijonoja ja tekstin muokkaamista tietokoneohjelmoinnissa.

## Kuinka tehdä

Seuraavissa koodiesimerkeissä näet, kuinka voit käyttää Rubya merkkijonon pituuden laskemiseen ja tulostamaan sen.

```Ruby
# Määrittele merkkijono
merkkijono = "Tämä on testi merkkijono"

# Käytä Rubyn .length -metodia laskemaan merkkijonon pituus
merkkijono_pituus = merkkijono.length

# Tulosta tulos
puts "Merkkijonon pituus on #{merkkijono_pituus} merkkiä."

# Output: Merkkijonon pituus on 25 merkkiä.
```

Voit myös käyttää .size -metodia, joka toimii samalla tavalla kuin .length:

```Ruby
# Määrittele merkkijono
merkkijono = "Tämä on toinen testi merkkijono"

# Käytä .size -metodia laskemaan merkkijonon pituus
merkkijono_pituus = merkkijono.size

# Tulosta tulos
puts "Merkkijonon pituus on #{merkkijono_pituus} merkkiä."

# Output: Merkkijonon pituus on 30 merkkiä.
```

## Syventyminen

Merkkijonon pituuden laskemisen lisäksi voit myös käyttää muita Ruby-menetelmiä käsittelemään merkkijonoja. Esimerkiksi voit käyttää .split -metodia jakamaan merkkijono useisiin osiin sen perusteella, mistä haluat sen jakaa. Voit myös käyttää .upcase ja .downcase -metodeja muuttamaan merkkijonon kirjainkoon.

Yksi tärkeä asia, joka kannattaa muistaa, on se, että Rubyssa merkkijonot ovat muuttumattomia. Tämä tarkoittaa, että kun olet luonut merkkijonon, sitä ei voi muokata. Sen sijaan voit käyttää erilaisia metodeja luomaan uusia merkkijonoja vanhojen pohjalta.

## Katso myös

- [Rubyn virallinen dokumentaatio merkkijonojen käsittelyyn](https://ruby-doc.org/core-2.6.6/String.html)
- [The Ruby Style Guide: merkkijonot](https://rubystyle.guide/#strings)