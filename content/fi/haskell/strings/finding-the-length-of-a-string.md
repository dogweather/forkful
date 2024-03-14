---
date: 2024-01-20 17:47:41.411420-07:00
description: "Mittaaminen tarkoittaa merkkijonon pituuden selvitt\xE4mist\xE4. Ohjelmoijat\
  \ tekev\xE4t sen, koska pituuden tiet\xE4minen on kriittist\xE4 kun k\xE4sitell\xE4\
  \xE4n tekstidataa \u2013\u2026"
lastmod: '2024-03-13T22:44:56.606800-06:00'
model: gpt-4-1106-preview
summary: "Mittaaminen tarkoittaa merkkijonon pituuden selvitt\xE4mist\xE4. Ohjelmoijat\
  \ tekev\xE4t sen, koska pituuden tiet\xE4minen on kriittist\xE4 kun k\xE4sitell\xE4\
  \xE4n tekstidataa \u2013\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
---

{{< edit_this_page >}}

## What & Why?
Mittaaminen tarkoittaa merkkijonon pituuden selvittämistä. Ohjelmoijat tekevät sen, koska pituuden tietäminen on kriittistä kun käsitellään tekstidataa – esimerkiksi validointiin, rajaukseen ja iterointiin.

## How to:
Haskellissa merkkijonon pituuden löytäminen on suoraviivaista. Käytä `length`-funktiota. 

```haskell
main :: IO ()
main = do
    let tervehdys = "Hei maailma!"
    print (length tervehdys) -- Tulostaa merkkijonon pituuden
```

Koodin ajaminen tulostaa `12`, koska "Hei maailma!" on 12 merkin pituinen.

## Deep Dive
Alun perin, Haskellin `length`-funktio oli osa standardikirjastoa. Se laskee listan alkiot, ja merkkijono Haskellissa on merkkilista.

Vaihtoehtoina, voit käyttää `Data.Text`-kirjastoa, joka tarjoaa tehokkaampia työkaluja tekstinkäsittelyyn. 

`Data.Text.length` on suorituskyvyltään parempi suurien merkkijonojen kanssa:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    let tervehdys = T.pack "Hei maailma!"
    print (T.length tervehdys)
```

Toteutukseltaan, `length` on rekursiivinen funktio. Se käy läpi listan, laskien yhden jokaisesta alkioista, kunnes lista on tyhjä. Suurien listojen kanssa tämä voi olla tehotonta, mikä selittää `Data.Text`-kirjaston suosion.

## See Also
- [Haskell `length` documentation](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:length)
- [`Data.Text` package](https://hackage.haskell.org/package/text) 
- [Performance comparison of string types in Haskell](https://stackoverflow.com/questions/16022417/performance-comparison-of-string-types-in-haskell)
