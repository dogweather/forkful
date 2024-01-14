---
title:    "Haskell: Väliaikaistiedoston luominen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi
Miksi luoda väliaikaisia tiedostoja ohjelmoinnissa? Väliaikaisten tiedostojen luominen voi olla hyödyllistä, kun tarvitset väliaikaisen tallennuspaikan ohjelmassasi. Voit esimerkiksi tallentaa käyttäjältä tulevan datan ensin väliaikaiseen tiedostoon ja käsitellä sen sitten myöhemmin.

## Miten
Luodaan väliaikainen tiedosto käyttäen Haskellin `System.IO.Temp` -moduulia:

```Haskell
import System.IO.Temp

main = withSystemTempFile "tiedostonimi.txt" $ \tempFilePath tempHandle -> do 
    -- suoritetaan halutut toimenpiteet tiedostolla käyttäen tempFilePath- ja tempHandle-muuttujia
```

Tässä esimerkissä `withSystemTempFile` -funktio ottaa ensimmäisenä parametrina halutun tiedostonimen ja toisena parametrina `IO` -toiminnon, jota halutaan suorittaa käytettävissä olevalla väliaikaisella tiedostolla. `tempFilePath` on polku väliaikaiseen tiedostoon ja `tempHandle` on tietovirta, jolla voit käsitellä tiedostoa.

Käytettäessä monimutkaisempia datarakenteita, kuten `ByteString` tai `Text`, voit käyttää `withSystemTempFile` -funktion sijasta `withSystemTempFile'` -funktiota, joka ottaa kolmannen parametrinä datan, jonka haluat kirjoittaa väliaikaiseen tiedostoon. Esimerkiksi:

```Haskell
import System.IO.Temp
import Data.ByteString

main = withSystemTempFile' "tiedostonimi.bin" $ \filePath handle -> do
    let dataToWrite = pack [72, 101, 108, 108, 111, 32, 72, 97, 115, 107, 101, 108, 108]
    hPut handle dataToWrite
```

## Syvempää tarkastelua
Haskellin `System.IO.Temp` -moduuli tuo käyttöön useita funktioita, joita voit käyttää väliaikaisten tiedostojen luomiseen ja käsittelyyn. Voit esimerkiksi luoda väliaikaisen hakemiston `withSystemTempDirectory` -funktion avulla tai poistaa väliaikaisia tiedostoja `removeTempDirectoryRecursive` -funktiolla.

Voit myös antaa omat haluamasi tiedostopäätteet väliaikaisilla tiedostoilla käyttämällä `openTempFileWithDefaultPermissions` -funktiota. Tämä on hyödyllistä jos haluat esimerkiksi luoda väliaikaisen `.txt` -tiedoston.

## Katso myös
- [Haskellin System.IO.Temp-moduulin dokumentaatio](https://hackage.haskell.org/package/temporary-1.3.4.1/docs/System-IO-Temp.html)
- [Haskell-opetusohjelma: Väliaikaisten tiedostojen hallinnointi](https://code.world/haskell#hwQshy2KkZIIC-R82L6z_a)
- [Haskell - virallinen sivusto](https://www.haskell.org/)