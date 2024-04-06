---
date: 2024-01-20 17:51:09.685955-07:00
description: "How to: - Miten tehd\xE4: Haskellissa merkkijonon interpolointi ei ole\
  \ yht\xE4 suoraviivaista kuin joissakin muissa kieliss\xE4, mutta se onnistuu kirjastoja\u2026"
lastmod: '2024-04-05T22:38:57.207685-06:00'
model: gpt-4-1106-preview
summary: "- Miten tehd\xE4: Haskellissa merkkijonon interpolointi ei ole yht\xE4 suoraviivaista\
  \ kuin joissakin muissa kieliss\xE4, mutta se onnistuu kirjastoja hy\xF6dynt\xE4\
  en. T\xE4ss\xE4 esimerkki `text`-kirjaston kanssa."
title: Merkkijonon interpolointi
weight: 8
---

## How to: - Miten tehdä:
Haskellissa merkkijonon interpolointi ei ole yhtä suoraviivaista kuin joissakin muissa kielissä, mutta se onnistuu kirjastoja hyödyntäen. Tässä esimerkki `text`-kirjaston kanssa:

```Haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import Data.Text.IO as TIO
import Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder.Int

main :: IO ()
main = do
    let age = 25
    let name = "Juha"
    let interpolated = Builder.toLazyText $ "Hei, nimeni on " <> Builder.fromText name <> " ja olen " <> decimal age <> " vuotta vanha."
    TIO.putStrLn interpolated
```

Ajaessasi tämän saat tulokseksi:

```
Hei, nimeni on Juha ja olen 25 vuotta vanha.
```

## Deep Dive - Syventyminen
Merkkijonon interpolointi on perinteisesti ollut osa dynaamisia kieliä, kuten Python tai Ruby. Haskellissa, joka on staattisesti tyypitetty kieli, interpolointi vaatii ylimääräisiä työkaluja tai kirjastoja, kuten `text`.

Vaihtoehtoisia tapoja suorittaa interpolointi Haskellissa ovat esimerkiksi `printf`-funktio perinteisestä `Text.Printf`-moduulista tai `formatting`-kirjasto, joka tarjoaa monipuolisempia muotoiluvaihtoehtoja.

Interpoloinnin toteutus vaatii usein ylimääräistä käännösaikaisia ominaisuuksia, kuten kirjaimellisten merkkijonojen ylilatausta (`OverloadedStrings`) tai laajennettuja kuvaajia (`QuasiQuotes`). Se, että interpolointi ei ole kielen ydinosassa, heijastaa Haskellin funktionaalista filosofiaa ja painotusta muuttumattomuuteen.

## See Also - Katso myös
- [Text.Printf moduuli](https://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Printf.html) - Toteuttaa C-tyylisen `printf`-funktion Haskellissa.
- [Formatting kirjasto](https://hackage.haskell.org/package/formatting) - Monipuolinen muotoilukirjasto Haskellin merkkijonoille.
- [Haskell `text`-kirjasto](https://hackage.haskell.org/package/text) - Tehokas käsittelemään Unicode-merkkijonoja Haskellissa.
