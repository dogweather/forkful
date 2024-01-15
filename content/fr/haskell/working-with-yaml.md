---
title:                "Travailler avec YAML"
html_title:           "Haskell: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des fichiers de configuration ou de données structurées, vous avez probablement déjà entendu parler de YAML. C'est un format de données léger et facile à utiliser qui est devenu populaire dans le développement de logiciels et la gestion de configuration. En utilisant Haskell, vous pouvez facilement travailler avec YAML pour créer, lire et modifier des fichiers de données.

## Comment faire

Tout d'abord, nous devons importer le package `Data.Yaml` dans notre code Haskell. Ensuite, nous pouvons utiliser la fonction `decodeFile` pour lire un fichier YAML et le convertir en un type de données Haskell.

```Haskell
import Data.Yaml (decodeFile)

-- Lecture d'un fichier YAML
main = do
  config <- decodeFile "/chemin/vers/le/fichier.yaml"

  -- Si la conversion a réussi, nous pouvons accéder aux données
  case config of 
    Just conf -> putStrLn $ "Configuration : " ++ show conf
    Nothing -> putStrLn "Erreur lors de la lecture du fichier YAML"
```

Pour écrire des données dans un fichier YAML, nous pouvons utiliser la fonction `encodeFile` :

```Haskell
-- Écriture dans un fichier YAML
main = do
  let config = [("nom", "John"), ("age", 30)] -- Données à écrire
  encodeFile "/chemin/vers/le/fichier.yaml" config
```

Il est également possible de travailler avec des données YAML en utilisant une représentation de type personnalisée en Haskell. Par exemple, si nous avons un fichier YAML contenant une liste d'utilisateurs avec des noms et des âges, nous pouvons créer un type de données `Utilisateur` correspondant :

```Haskell
import Data.Yaml (FromJSON, ToJSON, (.:), (.:?), withObject)

data Utilisateur = Utilisateur 
  { nom :: String
  , age :: Int
  } deriving (Show, Generic)

-- Implémentation des instances FromJSON et ToJSON pour notre type Utilisateur
instance FromJSON Utilisateur where
  parseJSON = withObject "utilisateur" $ \u -> do
    nom <- u .: "nom"
    age <- u .: "age"
    return Utilisateur { nom = nom, age = age }

instance ToJSON Utilisateur where
  toJSON (Utilisateur nom age) = object
    [ "nom" .= nom
    , "age" .= age
    ]
```

Maintenant, nous pouvons utiliser `decodeFile` pour lire notre fichier YAML dans une liste de valeurs `Utilisateur`. Nous pouvons également utiliser `encodeFile` pour écrire une liste de `Utilisateur` dans un fichier YAML. Cela peut être particulièrement utile si vous souhaitez manipuler des fichiers de configuration complexes avec des structures de données spécifiques en Haskell.

## Plongée en profondeur

La bibliothèque `Data.Yaml` contient d'autres fonctions utiles pour travailler avec YAML, comme `decode` et `encode` pour convertir des chaînes de caractères en données Haskell et inversement. Elle prend également en charge les types de données, tels que `ByteString` ou `Text`, pour travailler avec des fichiers YAML codés en différentes représentations de chaînes de caractères.

Il est également possible d'utiliser des options de configuration pour ajuster le comportement de la bibliothèque lors de la conversion de données. Par exemple, vous pouvez spécifier que les champs manquants doivent être ignorés ou que les valeurs `null` doivent être converties en valeurs par défaut dans votre type de données Haskell.

## Voir aussi

- [Documentation de la bibliothèque Data.Yaml en Haskell](https://hackage.haskell.org/package/yaml)
- [Exemple de manipulation de fichiers YAML en Haskell](https://www.stephendiehl.com/posts/yaml.html)
- [Tutoriel sur l'utilisation de YAML avec Haskell](https://devcenter.heroku.com/articles/haskell-yaml)