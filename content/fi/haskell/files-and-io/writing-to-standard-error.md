---
title:                "Kirjoittaminen standardivirheeseen"
aliases:
- /fi/haskell/writing-to-standard-error/
date:                  2024-02-03T19:33:20.131458-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Standardivirheeseen (stderr) kirjoittaminen Haskellissa mahdollistaa ohjelmien tulosteiden erottamisen normaalien tulosten ja virheilmoitusten välillä. Tämä on ratkaisevan tärkeää ongelmien signaloimiseksi ja vianmääritykseksi, ilman että se sotkee standarditulostetta (stdout), joka usein sisältää ohjelman päädatan tai tuloksen.

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
