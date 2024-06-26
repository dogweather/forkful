---
date: 2024-01-26 04:22:44.089246-07:00
description: "Hvordan: F\xF8rst, s\xF8rg for at du har et TOML parsingbibliotek. For\
  \ Haskell er `htoml` et popul\xE6rt valg. Du m\xE5 legge det til i prosjektets avhengigheter."
lastmod: '2024-03-13T22:44:40.866911-06:00'
model: gpt-4-0125-preview
summary: "F\xF8rst, s\xF8rg for at du har et TOML parsingbibliotek."
title: Jobbe med TOML
weight: 39
---

## Hvordan:
Først, sørg for at du har et TOML parsingbibliotek. For Haskell er `htoml` et populært valg. Du må legge det til i prosjektets avhengigheter.

```Haskell
-- Importer TOML parsingbiblioteket
import qualified Text.Toml as Toml

-- Definer din konfigurasjonsdatastruktur
data Config = Config {
  title :: String,
  owner :: Owner
} deriving (Show)

data Owner = Owner {
  name :: String,
  dob :: Maybe Day -- Valgfri dato
} deriving (Show)

-- Parsing en TOML-streng
main :: IO ()
main = do
  let tomlData = "[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00Z"
  case Toml.parseTomlDoc "" tomlData of
    Left err -> putStrLn $ "Feil: " ++ show err
    Right toml -> print toml -- Eller videre behandle den parsede TOML
```

Eksempelutdata kan struktureres og tilgås som hvilken som helst Haskell datatyp.

## Dypdykk
Historisk sett ble TOML skapt av Tom Preston-Werner, medstifter av GitHub, som en reaksjon på kompleksitetene til YAML og JSON for konfigurasjonsfiler. Det legger vekt på å være mer leselig og enklere å skrive enn JSON, og mer strengt og enkelt enn YAML.

Alternativer til TOML inkluderer JSON og YAML, hvor hvert format har sine egne styrker. JSON er allestedsnærværende og språkagnostisk, mens YAML tilbyr et mer menneskelesbart format. TOML verdsettes for sin enkelhet og konsistens, og unngår noen av fallgruvene til sine slektninger.

Implementasjon i Haskell innebærer typisk et bibliotek som parser TOML til en Haskell datatyp, ofte ved å utnytte Haskells avanserte typesystem for å sikre korrekthet. Parsing kan gjøres gjennom rekursiv nedstigning eller kombinatorparsing, som balanserer effektivitet med lesbarhet og vedlikehold av koden.

## Se Også
- `htoml`: https://hackage.haskell.org/package/htoml
- Offisielle TOML GitHub-repositorium: https://github.com/toml-lang/toml
- Sammenligning av data serialiseringsformater: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
