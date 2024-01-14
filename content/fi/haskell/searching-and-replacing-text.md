---
title:                "Haskell: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit etsiä ja korvata tekstiä Haskell-ohjelmoinnissa? Joskus sinun täytyy muuttaa useita samanlaisia merkkijonoja yhdellä kertaa, kuten vaihtaessasi nimen projektissa tai korvatessasi oikeassa taulukossa olevat tiedot testituloksilla.

## Kuinka

Helpoin tapa etsiä ja korvata tekstiä on käyttää `replace` -funktiota `Data.Text` -moduulista. Voit käyttää sitä seuraavasti:

```Haskell
import qualified Data.Text as T

replace "Haskell" "Rust" (T.pack "Opi Haskell:sta") 
```

Tämä tuottaa tuloksen `"Opi Rust:sta"`. Huomaa, että käytimme `T.pack` -funktiota muuntaaksemme tekstiä `Data.Text` -tyyppisiksi.

Voit myös käyttää säännöllisiä lausekkeita hakemaan ja korvaamaan tekstiä seuraavasti:

```Haskell
import qualified Data.Text as T
import Text.Regex.PCRE.Text (sub, gsub)

sub "a[bc]+" (const "def") (T.pack "aaabbb") -- "def"
gsub "[0-9]+" "x" (T.pack "1 2 3 4 5") -- "x x x x x"
```

Tässä `sub` -funktio korvaa ensimmäisen esiintymän annetulla merkkijonolla ja `gsub` -funktio korvaa kaikki esiintymät. Huomaa, että `Regex.PCRE.Text` -moduulissa on myös `subCapture` ja `gsubCapture` -funktiot, jotka tallentavat vastaavat kaapit uudelleenkirjoitettuun merkkijonoon.

## Syvemmälle

`Data.Text` -moduulilla on monia muita hyödyllisiä toimintoja, kuten `strip`, `words` ja `lines`, jotka voivat auttaa tekstien käsittelyssä. Voit myös käyttää `Data.ByteString` -moduulia, jos haluat tehdä toimintoja suorituskykyisemmin.

Katso dokumentaatiosta lisää `Data.Text` -ja `Data.ByteString` -toiminnoista sekä `Regex.PCRE.Text` -moduulin säännöllisten lausekkeiden funktioista.

## Katso myös

- [Haskellin dokumentaatio](https://www.haskell.org/documentation/)
- [Haskell-opetusohjelma](https://haskell.tips/)
- [Haskellin yhteisöfoorumi](https://discourse.haskell.org/)