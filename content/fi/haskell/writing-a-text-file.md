---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
"Mitä & Miksi?"

Tekstitiedoston kirjoittaminen tarkoittaa merkkijonojen tallentamista tiedostoon. Ohjelmoijat tekevät tätä tiedon pysyvää säilytystä, lokitietojen kirjaamista tai käyttöliittymän ulostulojen tallentamista varten.

## How to:
"Näin teet:"

Käytä `writeFile`-funktiota tiedoston kirjoittamiseen. Se korvaa tiedoston sisällön.

```Haskell
import System.IO

main :: IO ()
main = do
  let tiedosto = "tervehdys.txt"
  let sisalto = "Hei, Haskell!"
  writeFile tiedosto sisalto
```

Kun haluat liittää tekstiä tiedoston loppuun, käytä `appendFile`-funktiota.

```Haskell
main :: IO ()
main = do
  let tiedosto = "tervehdys.txt"
  let lisays = "\nLisää tekstiä."
  appendFile tiedosto lisays
```

## Deep Dive
"Sukellus syvemmälle"

Historia: Tekstitiedostojen käsittely on ollut ohjelmointikielten perustoiminnallisuuksia alusta asti. Haskellissa `System.IO`-moduuli tarjoaa funktioita tiedostojen käsittelyyn.

Vaihtoehdot: `writeFile` ja `appendFile` ovat suoraviivaisia, mutta monimutkaisempiin tarpeisiin `openFile`, `hPutStr`, ja `hClose` antavat enemmän kontrollia.

Toteutus: `writeFile` käyttää laiska evaluointia, eli tiedostoon kirjoitus tapahtuu vain, kun tiedostovirtaan kirjoitetaan tarpeeksi dataa tai virta suljetaan.

## See Also
"Lisää aiheesta"

- Learn You a Haskell for Great Good!: http://learnyouahaskell.com/
- Real World Haskell: http://book.realworldhaskell.org/
- Haskell dokumentaatio, `System.IO`: https://hackage.haskell.org/package/base/docs/System-IO.html