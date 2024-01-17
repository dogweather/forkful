---
title:                "Merkkijonon suurennus"
html_title:           "Haskell: Merkkijonon suurennus"
simple_title:         "Merkkijonon suurennus"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Isolla alkukirjaimella kirjoittaminen on yksinkertainen mutta tärkeä ohjelmointikäytäntö, jossa kaikki sanat alkavat isolla kirjaimella. Tämä on tärkeää, koska se lisää ohjelmakoodin luettavuutta ja helpottaa ymmärtämistä. Useimmat ohjelmoijat käyttävät tätä käytäntöä, joka on myös yleinen konventio Haskellissa.

## Miten:

```Haskell
capitalize :: String -> String
capitalize str = unwords $ map (\word -> toUpper (head word) : tail word) (words str)

main = do
    let str = "tämä on esimerkki lauseesta"
    putStrLn (capitalize str)

-- Output:
-- Tämä On Esimerkki Lauseesta
```

## Syväsukellus:

Isojen alkukirjainten käyttö on ollut käytössä jo pitkään, ja se juontaa juurensa jo vanhoilta ajoilta, jolloin käsinkirjoitettuja tekstejä oli vaikea lukea ilman isojen alkukirjainten käyttöä. Nykyään se on enemmänkin konventio, jotta koodin lukeminen olisi helpompaa.

On myös olemassa muita tapoja tehdä isojen alkukirjainten muunnos Haskellissa, kuten käyttämällä Data.Char-moduulia tai rekurssiivista funktiota. On tärkeää muistaa, että käytännössä on aina useita tapoja saavuttaa sama tavoite, ja on hyvä pysyä yhteisön käytännöissä, jotta koodisi olisi mahdollisimman ymmärrettävää muille ohjelmoijille.

## Katso myös:

- [Haskell-dokumentaatio: Data.Char-moduuli](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Advanced Haskell Series: Strings](https://www.fpcomplete.com/blog/2017/07/stringly-typed-haskell)
- [Haskell Style Guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)