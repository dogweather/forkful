---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:56.382469-07:00
description: "Werken met TOML omvat het parsen en genereren van TOML (Tom's Obvious,\
  \ Minimal Language) data met Haskell. Programmeurs doen dit om gemakkelijk\u2026"
lastmod: 2024-02-19 22:05:09.947098
model: gpt-4-0125-preview
summary: "Werken met TOML omvat het parsen en genereren van TOML (Tom's Obvious, Minimal\
  \ Language) data met Haskell. Programmeurs doen dit om gemakkelijk\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met TOML omvat het parsen en genereren van TOML (Tom's Obvious, Minimal Language) data met Haskell. Programmeurs doen dit om gemakkelijk configuratiebestanden of data-uitwisseling te beheren met sterke typegaranties en minimale syntax rompslomp.

## Hoe:
Zorg eerst dat je een TOML-parseerbibliotheek hebt. Voor Haskell is `htoml` een populaire keuze. Je moet het toevoegen aan de afhankelijkheden van je project.

```Haskell
-- Importeer de TOML-parseerbibliotheek
import qualified Text.Toml as Toml

-- Definieer je configuratiedatastructuur
data Config = Config {
  title :: String,
  owner :: Owner
} deriving (Show)

data Owner = Owner {
  name :: String,
  dob :: Maybe Day -- Optionele datum
} deriving (Show)

-- Een TOML-string parsen
main :: IO ()
main = do
  let tomlData = "[owner]\nname = \"Tom Preston-Werner\"\ndob = 1979-05-27T07:32:00Z"
  case Toml.parseTomlDoc "" tomlData of
    Left err -> putStrLn $ "Fout: " ++ show err
    Right toml -> print toml -- Of verwerk de geparseerde TOML verder
```

Voorbeelduitvoer kan gestructureerd en benaderd worden zoals elk Haskell-datatype.

## Diepgaande Duik
Historisch gezien werd TOML gecreëerd door Tom Preston-Werner, mede-oprichter van GitHub, als reactie op de complexiteiten van YAML en JSON voor configuratiebestanden. Het benadrukt beter leesbaar en eenvoudiger te schrijven te zijn dan JSON, en strenger en simpeler dan YAML.

Alternatieven voor TOML zijn onder andere JSON en YAML, waarbij elk formaat zijn eigen sterke punten heeft. JSON is alomtegenwoordig en taalonafhankelijk, terwijl YAML een meer leesbaar formaat biedt. TOML wordt gewaardeerd om zijn eenvoud en consistentie, waarbij het sommige valkuilen van zijn verwanten vermijdt.

Implementatie in Haskell omvat typisch een bibliotheek die TOML parseert naar een Haskell-datatype, vaak profiterend van het geavanceerde typesysteem van Haskell om correctheid te waarborgen. Parsen kan gedaan worden door recursieve afdaling of combinator-parsing, wat efficiëntie balanceert met leesbaarheid en onderhoudbaarheid van de code.

## Zie Ook
- `htoml`: https://hackage.haskell.org/package/htoml
- Officiële TOML GitHub-repository: https://github.com/toml-lang/toml
- Vergelijking van data-serialisatieformaten: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
