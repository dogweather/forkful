---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Vakiostandardivirhe (stderr) on erillinen tulostusväylä, joka on suunniteltu virheviestien käsittelyyn. Ohjelmoijat käyttävät sitä erottamaan normaalin tulostuksen virheviesteistä ja debuggausinformaatiosta.

## How to:
Haskellissa `hPutStrLn` ja `stderr` toimivat yhdessä virhetekstien kirjoittamiseen. Käytä `System.IO`-moduulia.

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Tämä on virheviesti"
```

Kun suoritat ohjelman, näet:

```
Tämä on virheviesti
```

Voit ohjata vain virheviestit tiedostoon komennolla `./ohjelma 2> virheet.log`.

## Deep Dive
Stderr juontaa juurensa Unix-järjestelmistä, jossa prosessilla on aina kolme perustiedostovirtaa: standardituloste (stdout), standardivirhe (stderr) ja standardisyöte (stdin). Haskellin `System.IO`-moduuli tarjoaa funktiot virrille kirjoittamiseen ja lukemiseen. Käytön helppous ja virheraportoinnin selkeys ovat keskeisiä syitä stderr:n käytölle. Vaihtoehtoisesti voi käyttää kirjastoa `System.Log.Logger`, jolloin saa enemmän lokitusvaihtoehtoja.

## See Also
- Hoogle `System.IO`: https://hoogle.haskell.org/?hoogle=System.IO
- Understand `stdin`, `stdout`, `stderr`: https://en.wikipedia.org/wiki/Standard_streams