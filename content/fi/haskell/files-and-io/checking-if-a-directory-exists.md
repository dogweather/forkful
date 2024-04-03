---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:37.950076-07:00
description: "Kuinka: Haskell tarjoaa peruskirjastonsa kautta suoraviivaisia tapoja\
  \ tarkistaa hakemiston olemassaolo, p\xE4\xE4asiassa k\xE4ytt\xE4m\xE4ll\xE4 `System.Directory`\u2026"
lastmod: '2024-03-13T22:44:56.629073-06:00'
model: gpt-4-0125-preview
summary: "Haskell tarjoaa peruskirjastonsa kautta suoraviivaisia tapoja tarkistaa\
  \ hakemiston olemassaolo, p\xE4\xE4asiassa k\xE4ytt\xE4m\xE4ll\xE4 `System.Directory`\
  \ -moduulia."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Kuinka:
Haskell tarjoaa peruskirjastonsa kautta suoraviivaisia tapoja tarkistaa hakemiston olemassaolo, pääasiassa käyttämällä `System.Directory` -moduulia. Katsotaanpa yksinkertaista esimerkkiä:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/path/to/your/directory"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Onko hakemisto olemassa? " ++ show exists
```

Esimerkkitulo, riippuen siitä, onko hakemisto olemassa:

```
Onko hakemisto olemassa? True
```
Tai:
```
Onko hakemisto olemassa? False
```

Monimutkaisemmissa skenaarioissa tai lisäominaisuuksien tarpeessa saatat harkita suosittua kolmannen osapuolen kirjastoa, kuten `filepath`, tiedostopolkuja käsittelemään ja manipuloimaan abstraktimmalla tavalla. Kuitenkin pelkästään hakemiston olemassaolon tarkistamisen tarkoitukseen peruskirjaston `System.Directory` riittää ja on tehokas.

Muista, että tiedostojärjestelmien kanssa työskentely voi vaihdella alustojen välillä, ja Haskellin lähestymistapa pyrkii abstrahoimaan joitakin näistä eroista. Testaa aina tiedostotoimintosi kohdejärjestelmässä varmistaaksesi odotetun käyttäytymisen.
