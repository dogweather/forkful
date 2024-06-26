---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:20.131458-07:00
description: "Kuinka: Haskellissa stderriin kirjoittaminen on suoraviivaista peruskirjaston\
  \ `System.IO`-moduulin avulla. Alla on perusesimerkki esittelyn\xE4."
lastmod: '2024-03-13T22:44:56.630972-06:00'
model: gpt-4-0125-preview
summary: Haskellissa stderriin kirjoittaminen on suoraviivaista peruskirjaston `System.IO`-moduulin
  avulla.
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Kuinka:
Haskellissa stderriin kirjoittaminen on suoraviivaista peruskirjaston `System.IO`-moduulin avulla. Alla on perusesimerkki esittelynä:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Tämä on virheilmoitus."
```

Tämän ohjelman tuloste stderriin olisi:

```
Tämä on virheilmoitus.
```

Jos työskentelet monimutkaisemmassa sovelluksessa tai tarvitset parempaa hallintaa lokitukseen (mukaan lukien virheet), saatat valita kolmannen osapuolen kirjaston. Yksi suosittu vaihtoehto on `monad-logger`, joka integroituu Haskell-ohjelmoinnin `mtl`-tyyliin. Tässä on pieni pätkä käyttäen `monad-logger`ia:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Tämä on virheilmoitus käyttäen monad-loggeria."
```

Kun ajetaan, `monad-logger`-versio tuottaa samoin virheilmoituksen, mutta se on varustettu enemmän kontekstilla kuten aikaleimoilla tai lokitasoilla, riippuen konfiguraatiosta:

```
[Error] Tämä on virheilmoitus käyttäen monad-loggeria.
```

Molemmat menetelmät palvelevat stderriin kirjoittamisen tarkoitusta, ja valinta riippuu suurelta osin sovelluksesi monimutkaisuudesta ja tarpeista.
