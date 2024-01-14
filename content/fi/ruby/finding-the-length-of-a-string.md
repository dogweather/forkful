---
title:    "Ruby: Merkkijonon pituuden löytäminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi

Miksi haluat selvittää merkkijonon pituuden ohjelmoinnin yhteydessä? Merkkijonojen pituuden laskeminen on tärkeä osa monia ohjelmointitehtäviä. Esimerkiksi merkkijonon pituus voi auttaa sinua määrittämään, kuinka monta merkkiä tarvitset tiettyyn laskelmaan tai kuinka paljon tilaa tarvitset merkkijonon tallentamiseen.

# Kuinka tehdä

Ruby:ssa merkkijonon pituuden laskeminen on helppoa käyttämällä `.length` -metodia. Tämä metodi antaa sinulle merkkijonon pituuden numerona. Katso alla oleva esimerkki:

```Ruby
merkkijono = "Tervetuloa"

puts merkkijono.length
```

Tulos:

```
11
```

Toinen tapa selvittää merkkijonon pituus on käyttää `.size` -metodia, joka toimii samalla tavalla kuin `.length` -metodi. Voit myös käyttää `.count` -metodia määrittämään tietyn merkin esiintymien määrän merkkijonossa, esimerkiksi:

```Ruby
merkkijono = "Tervetuloa"

puts merkkijono.count("o")
```

Tulos:

```
2
```

# Syvällinen sukellus

Ruby käyttää Unicodea merkkijonojen tallentamiseen, mikä tarkoittaa, että jokainen merkki voi sisältää useamman kuin yhden tavun tai bittijonon. Tämän vuoksi merkkijonon pituuden laskeminen voi olla monimutkaisempaa kuin miltä se näyttää. Tätä haastetta varten Ruby tarjoaa `.bytesize` -metodin, joka antaa merkkijonon todellisen koon tavuina.

On myös tärkeää huomata, että `.length` -metodi ei toimi aina oikein moniosaisilla merkkiyhdistelmillä, kuten emoji. Tässä tapauksessa haluat ehkä käyttää `.grapheme_clusters` -metodia, joka laskee yhdistelmät oikein.

# Katso myös

- [Ruby Ohjeistus - Merkkijonon pituuden laskeminen](https://ruby-doc.org/core-3.0.1/String.html#method-i-length)
- [Ruby Ohjeistus - Merkkijonon merkkien laskeminen](https://ruby-doc.org/core-3.0.1/String.html#method-i-count)
- [Ruby Ohjeistus - Merkkijonon koon laskeminen tavuina](https://ruby-doc.org/core-3.0.1/String.html#method-i-bytesize)