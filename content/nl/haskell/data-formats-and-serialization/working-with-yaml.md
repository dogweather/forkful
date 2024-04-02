---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:44.366180-07:00
description: "YAML (YAML Ain't Markup Language) is een door mensen leesbaar gegevensserialisatieformaat.\
  \ Programmeurs gebruiken het voor configuratiebestanden en\u2026"
lastmod: '2024-03-13T22:44:50.872852-06:00'
model: gpt-4-0125-preview
summary: "YAML (YAML Ain't Markup Language) is een door mensen leesbaar gegevensserialisatieformaat.\
  \ Programmeurs gebruiken het voor configuratiebestanden en\u2026"
title: Werken met YAML
weight: 41
---

## Wat & Waarom?

YAML (YAML Ain't Markup Language) is een door mensen leesbaar gegevensserialisatieformaat. Programmeurs gebruiken het voor configuratiebestanden en gegevensuitwisseling vanwege de leesbaarheid en eenvoud.

## Hoe te:

Om met YAML in Haskell te werken, gebruik je het `yaml` pakket. Installeer het eerst:

```shell
cabal install yaml
```

Definieer een gegevensstructuur en codeer en decodeer vervolgens YAML:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.YAML
import Data.ByteString (ByteString)

-- Definieer een gegevensstructuur
data Person = Person
    { name :: String
    , age  :: Int
    } deriving (Show)

-- Een voorbeeld van een Person instantie
examplePerson :: Person
examplePerson = Person "Chris" 30

-- Serialisatie (Haskell naar YAML)
yamlEncode :: Person -> ByteString
yamlEncode = encode

-- Deserialisatie (YAML naar Haskell)
yamlDecode :: ByteString -> Either String Person
yamlDecode = decodeThrow

main :: IO ()
main = do
    -- Codeer naar YAML en print het resultaat
    putStrLn "Gecodeerd YAML:"
    print $ yamlEncode examplePerson
  
    -- Voorbeeld YAML data
    let exampleYAML = "name: Alex\nage: 25\n"
  
    -- Decodeer van YAML en print het resultaat
    putStrLn "Gedecodeerd Haskell:"
    print $ yamlDecode exampleYAML
```

Voorbeelduitvoer voor coderen en decoderen:

```plaintext
Gecodeerd YAML:
"age: 30\nname: Chris\n"
Gedecodeerd Haskell:
Right (Person {name = "Alex", age = 25})
```

## Diepgaande duik

YAML is gestart in 2001, gericht op gegevensserialisatie en leesbaarheid voor mensen. Het is een populaire keuze voor configuratiebestanden, zoals Docker Compose en GitHub Workflows. Alternatieven zijn onder andere JSON en XML, maar de minimale syntaxis van YAML wordt vaak de voorkeur gegeven vanwege de schone verschijning. Bij het implementeren van YAML in Haskell is de sleutel het definiëren van gegevensstructuren die overeenkomen met de sleutel-waardeparen van YAML. Het `yaml` pakket, gebouwd op de libyaml C bibliotheek, biedt solide prestaties en compatibiliteit.

## Zie ook

- Officiële YAML website: [https://yaml.org](https://yaml.org)
- `yaml` pakket op Hackage: [https://hackage.haskell.org/package/yaml](https://hackage.haskell.org/package/yaml)
- `aeson` pakket, voor JSON in Haskell dat overeenkomsten heeft: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
