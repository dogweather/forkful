---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
aliases:
- /fi/haskell/capitalizing-a-string/
date:                  2024-02-03T19:05:21.651103-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja Miksi?
Merkkijonon alkukirjaimen muuttaminen isoksi kirjaimeksi samalla kun varmistetaan, että loput kirjaimet pysyvät pieninä, on merkkijonon pääomanlisointia. Ohjelmoijat tekevät tämän muotoilun tulosteiden, kieliopillisen oikeellisuuden tekstissä tai tuotetun datan luettavuuden parantamiseksi.

## Miten:
Haskellissa voit pääomanlisoida merkkijonon käyttämällä standardikirjastoa ilman, että tarvitset kolmannen osapuolen kirjastoja.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Esimerkki käytöstä:
main = putStrLn $ capitalize "hello world"
```

Tuloste:
```
Hello world
```

Monimutkaisemmissa skenaarioissa tai käytön helpottamiseksi saattaisit haluta käyttää kolmannen osapuolen kirjastoa, kuten `text`, joka on suosittu tehokkaaseen merkkijonon käsittelyyn Haskellissa.

Ensin sinun täytyy lisätä `text` projektisi riippuvuuksiin. Sen jälkeen voit käyttää sen funktioita pääomanlisoimaan merkkijonon seuraavasti:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Esimerkki käytöstä text-kirjaston kanssa:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Tuloste:
```
Hello world
```

Molemmat näistä esimerkeistä osoittavat yksinkertaisia, mutta tehokkaita tapoja pääomanlisoida merkkijono Haskellissa, kolmannen osapuolen kirjastojen kanssa tai ilman.
