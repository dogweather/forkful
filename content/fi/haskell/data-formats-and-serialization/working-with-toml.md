---
date: 2024-01-26 04:22:44.683304-07:00
description: "Kuinka: Ensin, varmista ett\xE4 sinulla on TOML-j\xE4sennyskirjasto.\
  \ Haskellille `htoml` on suosittu valinta. Sinun on lis\xE4tt\xE4v\xE4 se projektisi\
  \ riippuvuuksiin."
lastmod: '2024-03-13T22:44:56.637731-06:00'
model: gpt-4-0125-preview
summary: "Ensin, varmista ett\xE4 sinulla on TOML-j\xE4sennyskirjasto."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

## Kuinka:
Ensin, varmista että sinulla on TOML-jäsennyskirjasto. Haskellille `htoml` on suosittu valinta. Sinun on lisättävä se projektisi riippuvuuksiin.

```Haskell
-- Tuo TOML-jäsennyskirjasto
import qualified Text.Toml as Toml

-- Määrittele konfiguraatio datarakenteesi
data Config = Config {
  title :: String,
  owner :: Owner
} deriving (Show)

data Owner = Owner {
  name :: String,
  dob :: Maybe Day -- Vapaaehtoinen päivämäärä
} deriving (Show)

-- TOML-merkkijonon jäsentäminen
main :: IO ()
main = do
  let tomlData = "[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00Z"
  case Toml.parseTomlDoc "" tomlData of
    Left err -> putStrLn $ "Virhe: " ++ show err
    Right toml -> print toml -- Tai jatka jäsentynyttä TOML:ää
```

Näyteulostulo voidaan rakentaa ja käyttää kuten mikä tahansa Haskellin datatyyppi.

## Syväsukellus
Historiallisesti TOML:n loi Tom Preston-Werner, GitHubin yhteisperustaja, reaktiona YAML:n ja JSON:n monimutkaisuuksiin konfiguraatiotiedostoissa. Se korostaa olevansa luettavampi ja helpompi kirjoittaa kuin JSON, sekä tiukempi ja yksinkertaisempi kuin YAML.

Vaihtoehtoja TOML:lle ovat JSON ja YAML, joilla kullakin on omat vahvuutensa. JSON on kaikkialla läsnäoleva ja kieliagnostinen, kun taas YAML tarjoaa ihmisen luettavamman muodon. TOML arvostetaan sen yksinkertaisuuden ja johdonmukaisuuden vuoksi, välttäen joitakin sen sukulaisformaattejen ongelmia.

Toteutus Haskellissa tyypillisesti sisältää kirjaston, joka jäsentää TOML:n Haskellin datatyypiksi, usein hyödyntäen Haskellin edistynyttä tyyppijärjestelmää korrektiuden varmistamiseksi. Jäsentäminen voidaan suorittaa rekursiivisen laskun tai kombinaattorijäsennyksen kautta, joka tasapainottaa tehokkuutta koodin luettavuuden ja ylläpidettävyyden kanssa.

## Katso myös
- `htoml`: https://hackage.haskell.org/package/htoml
- Virallinen TOML GitHub-repositorio: https://github.com/toml-lang/toml
- Tietojen sarjallistamismuotojen vertailu: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
