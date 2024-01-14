---
title:                "Haskell: Merkkijonon suuraakkostaminen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuttaa merkkijonon isoiksi kirjaimiksi Haskell-ohjelmointikielen avulla? Syitä tähän voivat olla esimerkiksi tekstin muotoilu, tietokannan hakujen tehostaminen tai tietojen yhdistäminen eri järjestelmistä.

## Miten tehdä

Ohjelmoinnin aloittelijan on helppo muuttaa merkkijono isoiksi kirjaimiksi Haskellissa. Tarvitset vain muutaman rivin koodia ja pääset tavoitteeseen.

```Haskell
import Data.Char

capitalize :: String -> String
capitalize = map toUpper

main = do
    putStrLn "Syötä merkkijono:"
    string <- getLine
    let capitalized = capitalize string
    putStrLn ("Isot kirjaimet: " ++ capitalized)

```

Esimerkiksi syötteellä "haskell on mahtava ohjelmointikieli" ohjelma tuottaa seuraavan tulosteen:

```Haskell
Syötä merkkijono:
haskell on mahtava ohjelmointikieli
Isot kirjaimet: HASKELL ON MAHTAVA OHJELMOINTIKIELI
```

## Syventävä sukellus

Merkkijonon isoiksi muuttaminen ei ole aina yksiselitteistä, sillä eri kielissä voi olla eroja siinä, miten merkistöjä käsitellään. Esimerkiksi Haskellissa `toUpper`-funktio käyttää Unicode-merkistöä, joten se muuttaa myös kansainväliset merkit isoiksi kirjaimiksi. Tämä tulee ottaa huomioon, jos käsitellään tekstiä, jossa on eri kielten merkkejä.

Myös tehokkuus voi olla tärkeä tekijä, kun kyseessä on suurempien merkkijonojen käsittely. Siksi kannattaa tutustua muihin vaihtoehtoihin, kuten `ByteString`-kirjastoon, joka tarjoaa nopeampia tapoja muuttaa merkkijonon merkkejä.

## Katso myös

- [Haskellin dokumentaatio merkkijonojen käsittelystä](https://www.haskell.org/tutorial/strings.html)
- [Data.Char-kirjaston dokumentaatio](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [ByteString-kirjaston dokumentaatio](https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString.html)