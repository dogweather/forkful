---
title:    "Haskell: Merkkijonon muuntaminen pienaakkosiksi"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Olet ehkä kuullut termistä "muuntaa merkkijono pienemmiksi kirjaimiksi" tai olet saattanut törmätä tähän tehtävään jossain Haskell-koodissa. Tässä blogipostauksessa käymme läpi miksi ja miten merkkijonoa kannattaa muuttaa pienemmiksi kirjaimiksi Haskellissa.

## Kuinka

String-tyyppi Haskellissa on immutaabeli, mikä tarkoittaa, että sitä ei voi muokata suoraan. Tämä tarkoittaa, että kun haluamme muuttaa merkkijonon kirjaimet pienemmiksi, meidän täytyy luoda uusi merkkijono, jossa alkuperäinen merkkijono on muuttunut. Tämä onnistuu käyttämällä `map`-funktiota, joka ottaa argumenteikseen muunnoksen funktiokutsun sekä merkkijonon. Muunnoksena toimii `toLower`-funktio, joka muuttaa merkkijonon kirjaimet pienemmiksi. Esimerkiksi:

```Haskell
map toLower "Tämä On MerkkiJono"  -- Output: "tämä on merkkijono"
```

Huomaa, että tämä toimii vain ASCII-koodatulle tekstille. Jos haluat muuntaa myös kansainvälisiä merkkejä, kannattaa käyttää `Data.Text`-moduulin tarjoamia funktioita, kuten `Data.Text.toLower`.

## Syvempi Sukellus

Käyttämällä `map`-funktiota voimme muuntaa merkkijonon kirjaimet pienemmiksi, mutta miten se oikeastaan toimii taustalla? Haskellissa on paljon tapoja käsitellä listoja, ja `map` on yksi niistä. Se ottaa listan ja muuntaa jokaisen listan elementin käyttäen sille määriteltyä funktiota. Tässä tapauksessa `map` käy läpi jokaisen merkin merkkijonossa ja käyttää sille `toLower`-funktiota.

## Katso Myös

- [Data.Text -moduulin dokumentaatio](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Listan operaattorit Haskellissa](https://wiki.haskell.org/List_operators)