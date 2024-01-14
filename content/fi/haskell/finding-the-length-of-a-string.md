---
title:                "Haskell: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi selvittää merkkijonon pituuden? Pituuden selvittäminen voi olla hyödyllistä esimerkiksi kirjoittaessaan tekstipohjaista sovellusta, jossa tarkkaillaan syötteiden pituutta tai analysoitaessa tekstidataa.

## Miten

Merkkijonon pituuden selvittäminen Haskell-kielen avulla on helppoa. Käytännössä tarvitset vain yhden funktion, nimeltään "length".

```Haskell
length "Tämä on esimerkkiteksti." 
```

```
25
```
Funktion "length" oletusarvo on laskeminen merkkien määrä, mutta voit myös käyttää sitä muiden tietotyyppien, kuten listojen, pituuden laskemiseen.

```Haskell
length [1, 2, 3, 4, 5]
```

```
5
```

## Syvällinen tarkastelu

Haskellissa merkkijonot ovat omia tietotyyppejä, mutta ne voivat myös olla listoja merkeistä. Tämä johtaa siihen, että funktion "length" kautta myös merkkijonon pituus lasketaan listan pituutena.

Tämän lisäksi Haskellissa on myös muita tapoja lukea merkkijonon pituutta, kuten käyttämällä funktiota "fst" ja "snd" tupleille, joissa ensimmäinen alkio on merkkijono ja toinen on sen pituus.

## Katso myös

- [Haskellin dokumentaatio merkkijonojen käsittelystä](https://www.haskell.org/onlinereport/string.html)
- [Haskellin opetusohjelma "Learn You a Haskell"](http://learnyouahaskell.com/starting-out#im-a-list-comprehension)