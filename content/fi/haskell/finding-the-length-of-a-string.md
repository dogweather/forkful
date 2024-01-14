---
title:    "Haskell: Merkkijonon pituuden löytäminen."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit etsiä merkkijonon pituuden? Yksi syy voi olla, että se on yleinen tehtävä ohjelmoinnissa ja useimmat ohjelmointikielten tarjoavat valmiita funktioita tämän tehtävän suorittamiseen. Haluat ehkä myös tietää, miten tämä tehtävä voidaan suorittaa käyttäen Haskell-kieltä.

## Miten

Onneksi Haskell tarjoaa helpon ja tehokkaan tavan etsiä merkkijonon pituus. Voit tehdä tämän käyttäen `length`-funktiota. Tämä funktio ottaa merkkijonon ja palauttaa sen pituuden.

```
Haskell
length "Tämä on esimerkki merkkijonosta"
```

Tulos: `33`

Voit myös käyttää tätä funktiota muuttujien ja lausekkeiden kanssa.

```
Haskell
let sana = "tehtävä"
length (sana ++ " " ++ "ohjelmointi")
```

Tulos: `22`

Näiden esimerkkien perusteella voit helposti käyttää `length`-funktiota ja saada merkkijonon pituus.

## Syvällinen sukellus

Tarkennetaan hieman, mitä tapahtuu kun kutsutaan `length`-funktiota. Haskell-kielessä `length` on osa `Prelude`-kirjastoa, joten se on käytettävissä ilman erillistä importtaamista. Tämä funktio hyväksyy yhden argumentin, joka voi olla mikä tahansa tyyppi `Foldable`, kuten lista, mansetti tai merkkijono.

Haskellin sisäisen toteutuksen kannalta `length`-funktio laskee vain argumenttinsa elementtien määrän. Tämä tarkoittaa, että käyttäessäsi `length`-funktiota merkkijonoon, se yksinkertaisesti laskee merkkien määrän. Tämän takia voit myös käyttää tätä funktiota muissa tyypeissä, kuten listoissa.

## Katso myös

- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)
- [Tietoa merkkijonoista Haskellissa](https://www.schoolofhaskell.com/user/Saecki/strings-in-haskell)
- [Funktionaalisen ohjelmoinnin tärkeimmät käsitteet](https://dev.to/saecki/the-most-important-concepts-of-functional-programming-4a1o)