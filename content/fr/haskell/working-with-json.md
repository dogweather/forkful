---
title:                "Travailler avec json"
html_title:           "Haskell: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi vous devriez apprendre à travailler avec JSON en Haskell. Eh bien, il y a plusieurs raisons ! Premièrement, JSON est un format de données extrêmement populaire et largement utilisé pour l'échange de données sur le Web. En travaillant avec JSON en Haskell, vous pourrez facilement interagir avec des API et des services Web. Deuxièmement, Haskell est un langage de programmation fonctionnel puissant et sûr, ce qui en fait un excellent choix pour manipuler des données complexes telles que JSON.

## Comment faire

Pour travailler avec JSON en Haskell, nous allons utiliser un package appelé *Aeson*. Ce package fournit des fonctions et des types qui rendent la manipulation de données JSON très simple et élégante. Pour commencer, nous devons tout d'abord installer le package en utilisant la commande suivante dans notre terminal :

```
cabal install aeson
```

Ensuite, dans notre fichier Haskell, nous allons importer le module *Data.Aeson* :

```
import Data.Aeson
```

Pour lire un fichier JSON dans notre code, nous pouvons utiliser la fonction `decodeFileStrict` de *Data.Aeson* :

```
fileContent <- decodeFileStrict "exemple.json" :: IO (Maybe Value)
```

Cette fonction renvoie un type `Maybe Value`, donc nous pouvons utiliser un pattern matching pour extraire les données si le fichier JSON a été correctement décodé :

```
case fileContent of
    Just json -> -- faire quelque chose avec les données JSON ici
    Nothing -> error "Impossible de décoder le fichier JSON"
```

Pour accéder à des données spécifiques dans notre fichier JSON, nous pouvons utiliser le type `Value` et des opérateurs tels que `.!` et `.!?` :

```
let name = json .! "name" :: Text -- pour accéder à une valeur de type Text
let age = json .!? "age" :: Maybe Int -- pour accéder à une valeur de type Maybe Int
```

## Plongée en profondeur

En travaillant avec JSON en Haskell, il est également utile de savoir comment encoder des valeurs Haskell en JSON. Pour ce faire, nous pouvons utiliser la fonction `encode` de *Data.Aeson* :

```
let person = object ["name" .= "Bob", "age" .= 25] -- crée un objet JSON avec des paires clé-valeur
let encodedPerson = encode person -- encode l'objet en JSON
```

Nous pouvons également utiliser des types personnalisés en utilisant la dérivation de type générique (GHC's `DeriveGeneric`) et en créant des instances pour les types `ToJSON` et `FromJSON` :

```
import GHC.Generics

data Person = Person
    { name :: Text
    , age :: Int
    } deriving (Generic, Show)

instance ToJSON Person
instance FromJSON Person
```

Maintenant, nous pouvons utiliser notre type `Person` avec les fonctions `encode` et `decodeFileStrict` pour manipuler des données JSON de manière plus structurée et sûre.

## Voir aussi

Pour en savoir plus sur la manipulation de données JSON en Haskell, vous pouvez consulter les liens suivants :

- [Documentation officielle de Aeson](https://hackage.haskell.org/package/aeson)
- [Tutoriel pour travailler avec JSON en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/working-with-json)
- [Didacticiel vidéo sur Aeson et l'encodage JSON](https://www.youtube.com/watch?v=w9pFQFDyzU0)