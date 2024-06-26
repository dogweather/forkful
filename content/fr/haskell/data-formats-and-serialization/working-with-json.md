---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:59.017489-07:00
description: "Comment faire : Haskell n'a pas de support int\xE9gr\xE9 pour le JSON\
  \ comme JavaScript, mais avec l'aide de biblioth\xE8ques tierces telles que **Aeson**,\
  \ la\u2026"
lastmod: '2024-03-13T22:44:57.856130-06:00'
model: gpt-4-0125-preview
summary: "Haskell n'a pas de support int\xE9gr\xE9 pour le JSON comme JavaScript,\
  \ mais avec l'aide de biblioth\xE8ques tierces telles que **Aeson**, la manipulation\
  \ du JSON devient simple."
title: Travailler avec JSON
weight: 38
---

## Comment faire :
Haskell n'a pas de support intégré pour le JSON comme JavaScript, mais avec l'aide de bibliothèques tierces telles que **Aeson**, la manipulation du JSON devient simple. Aeson fournit à la fois des fonctions de haut niveau et de bas niveau pour l'encodage (convertir des valeurs Haskell en JSON) et le décodage (parser du JSON en valeurs Haskell).

### Installer Aeson
D'abord, ajoutez Aeson aux dépendances de votre projet en mettant à jour votre fichier `.cabal` ou en utilisant directement Stack ou Cabal :

```shell
cabal update && cabal install aeson
```
ou, si vous utilisez Stack :
```shell
stack install aeson
```

### Parser du JSON
Commençons par un exemple basique de décodage de données JSON en un type Haskell. Supposons que nous ayons le JSON suivant représentant une personne :

```json
{
  "name": "John Doe",
  "age": 30
}
```

D'abord, définissez un type de données Haskell correspondant et faites-en une instance de `FromJSON` :

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- Fonction pour décoder JSON depuis un fichier
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Utilisation :
En supposant que `person.json` contient les données JSON montrées ci-dessus, exécutez :
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Sortie d'exemple :
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Encodage de valeurs Haskell en tant que JSON
Pour convertir une valeur Haskell de retour en JSON, vous devez faire de votre type une instance de `ToJSON` puis utiliser `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- En supposant le type Person de précédemment

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Sortie d'exemple :
```json
{"name":"Jane Doe","age":32}
```

Ces exemples montrent les bases de la manipulation de JSON en Haskell à l'aide d'Aeson. Rappelez-vous, Aeson offre bien plus, incluant des règles de parsing personnalisées, travailler avec du JSON complexe et imbriqué, et bien plus encore, adapté à divers besoins et scénarios.
