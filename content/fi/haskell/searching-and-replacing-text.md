---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Haskell: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Haskell on funktionaalinen ohjelmointikieli, joka korostaa puhtaita funktioita ja välttää muuttuvaa tilaa. Tämä tekee siitä erinomaisen työkalun merkkijonojen muokkaamiseen, kuten tekstin etsimiseen ja korvaamiseen.

## Kuinka tehdä

Haskellilla tekstien etsiminen ja korvaaminen voidaan tehdä helposti, käyttämällä valmiita funktioita ja heittämällä syntaksiakaan. Alla on esimerkki, jossa muutetaan teksti "Hello World" muotoon "Hello Haskell".

```Haskell
import Data.Text (replace)

main = do
  let text = "Hello World"
  let modifiedText = replace "World" "Haskell" text
  print modifiedText
```

Tämän koodin tulostus olisi:

```Haskell
"Hello Haskell"
```

Kuten huomaat, Haskellissa ei tarvitse aloittaa muuttujia tyyppiannotaatioilla. Kirjasto Data.Text tarjoaa funktion replace, joka ottaa ensimmäisenä parametrina etsittävän tekstin, toisena parametrina korvaavan tekstin ja lopulta haettavan tekstin.

## Syvällinen sukellus

Haskellin Data.Text-kirjasto tarjoaa monia muita hyödyllisiä funktioita tekstien muokkaamiseen, kuten capitalize, take, drop jne. Lisäksi Haskellilla on myös mahdollista käyttää säännöllisiä lausekkeita tekstien etsimiseen ja korvaamiseen.

Kannattaa myös tutustua listaan sisäänrakennettuja funktioita ja käyttää niitä hyväksi etsimisprosessissa. Esimerkiksi map-funktio, joka ottaa vastaan listan ja funktiokutsun, voisi auttaa muokkaamaan tekstiä haluttuun muotoon.

## Katso myös

- [Haskellin virallinen sivusto](https://www.haskell.org/)
- [Data.Text-kirjasto Hooglesta](https://hackage.haskell.org/package/text)
- [Haskelliin tutustumisen opas](https://wiki.haskell.org/Learning_Haskell)