---
title:                "Arbete med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"

category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett dataformat likt JSON, men mer läsbart. Programmerare använder det för konfigurationsfiler och datautbyte eftersom det är enkelt att läsa och skriva.

## How to:
I Haskell, använder vi biblioteket `yaml` för att hantera YAML-data. Här är en snabbstart:

```haskell
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  let yamlData = "- Haddock\n- Carp\n- Plaice\n"
  let decoded = decodeEither' (BS.pack yamlData) :: Either ParseException [String]
  print decoded
```
Kör detta, du borde få:

```plaintext
Right ["Haddock","Carp","Plaice"]
```

## Deep Dive
YAML, som "YAML Ain't Markup Language", skapades som ett mer människoläsbart alternativ till XML. Det har använts sedan tidigt 2000-tal. Alternativ inkluderar JSON och TOML. I Haskell, hanterar vi YAML genom att parse:a det till datatyper vi definierar. För detta använder vi oftast `yaml`-biblioteket som är ovanpå `libyaml`-biblioteket, en C-implementation.

## See Also
- YAML specifikation: https://yaml.org/spec/1.2.2/
- `yaml` biblioteket på Hackage: https://hackage.haskell.org/package/yaml
- `aeson` biblioteket för JSON: https://hackage.haskell.org/package/aeson
- TOML: https://toml.io/en/
