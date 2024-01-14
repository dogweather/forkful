---
title:    "Haskell: Merkkijonon pituuden löytäminen"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Miksi kukaan haluaisi selvittää merkkijonon pituuden? Yksinkertaisesti sanottuna, se on tärkeä perustaito ohjelmoinnissa. Monta kertaa meille tulee vastaan merkkijonoja, kuten käyttäjän syötteitä tai tiedostoja, ja niiden käsittely on välttämätöntä monissa sovelluksissa.

## Miten?

```Haskell
-- Määritellään funktio merkkijonon pituuden laskemiseksi
length :: String -> Int
length str = sum [1 | _ <- str]
```

Alla on esimerkki koodin toiminnasta ja sen tuottamasta tulosteesta:

```Haskell
length "Tämä on merkkijono"
--> 20
```

Edellä olevassa koodissa käytetään listan listansiirtymää `_` ja listan tiivistämistä `[1 | _ <- str]` laskeaksesi jokaisen listan arvon pituuden yhdeksi ja summataksesi ne yhteen. Tämä antaa meille merkkijonon pituuden Int-muodossa. Voit myös käyttää valmista `length` -funktiota, kuten alla olevassa esimerkissä:

```Haskell
length "Hello world!"
--> 12
```

## Syvempi muistiinpano

Merkkijonon pituuden laskennassa on tärkeää huomata, että se voi vaihdella eri kielistä ja merkistöistä riippuen. Esimerkiksi japaninkielisessä merkkijonossa käytetään kahta bittiä jokaisen merkin esittämiseen, mikä vaikuttaa merkkijonon kokonaispituuteen.

Lisäksi, vaikka ad hoc -fanit voivat käyttää listan pituuden laskemisen `length`-funktiota, joissakin tekniikoissa sitä ei suositella, koska se käyttää iteraatiota ja on siten hidas. Suositeltavaa on käyttää `Data.Text` -moduulia, joka on tarkoitettu erityisesti merkkijonojen käsittelyyn ja tarjoaa nopeamman `length` -funktion.

## Katso myös

- [Haskell-ohjelmointikielen virallinen verkkosivusto] (https://www.haskell.org)
- [Data.Text-moduuli] (https://hackage.haskell.org/package/text/docs/Data-Text.html)