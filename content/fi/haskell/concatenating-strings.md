---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen yhdistäminen tarkoittaa useiden erillisten merkkijonojen liittämistä yhteen loogisen kokonaisuuden muodostamiseksi. Ohjelmoijat tekevät näin tehdäkseen ohjelmakoodistaan entistä yksinkertaisemman ja informatiivisemman.

## Miten se tehdään:

Yhdistämme merkkijonot Haskellissa (++) operaattorilla. Katsotaan esimerkki:

```Haskell
let sana1 = "Hei"
let sana2 = ", maailma!"
let tervehdys = sana1 ++ sana2
print tervehdys
```

Tämän koodipätkän tulostus on:

```Haskell
"Hei, maailma!"
```

## Syvällisempi tarkastelu:

Historiallinen konteksti: Haskellin merkkijonot ovat listoja merkkejä ja tämä on periytynyt funktionaalisen ohjelmoinnin alkuaikojen lisp-kielestä.

Vaihtoehtoja: Concat-funktio on toinen tapa yhdistää merkkijonoja Haskellissa. Tämä on hyödyllinen, kun meillä on useita merkkijonoja listassa ja haluamme liittää ne yhteen.

Toteutuksen yksityiskohdat: Yhdistämisoperaattoria (++) tai concat-funktiota käytettäessä jokaisen merkin pitää olla samaa tyyppiä. Tämä tarkoittaa, että jos meillä on esimerkiksi lista numeroita, meidän täytyy muuttaa ne merkkijonoiksi ennen yhdistämistä.

## Katso myös:

2. [Haskellin listaoperaattorit](http://learnyouahaskell.com/starting-out#an-intro-to-lists)
3. [Haskellin Concat-funktion dokumentaatio](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:concat)