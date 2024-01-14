---
title:                "Haskell: Travailler avec le yaml"
simple_title:         "Travailler avec le yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Haskell cherchant à travailler avec des données structurées, il est possible que vous ayez entendu parler de YAML. YAML est un format de données simple et lisible pour les humains, tout en étant facilement interprétable par les machines. Mais pourquoi devriez-vous choisir YAML pour votre prochain projet? Jetons un coup d'œil.

## Comment faire

Avant de commencer à travailler avec YAML en Haskell, vous devez importer le module `Data.YAML`. Ensuite, vous pouvez utiliser la fonction `encode` pour convertir des données en YAML, et `decode` pour convertir du YAML en données Haskell.

Dans l'exemple suivant, nous définissons une liste de numéros entiers et utilisons `encode` pour les convertir en YAML. Ensuite, nous utilisons `decode` pour convertir le YAML en une liste de valeurs Haskell.

```Haskell
import Data.YAML

numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

main = do
  let yml = encode numbers
  print yml
  print $ decode yml :: Maybe [Int]
```

La sortie de ce code sera:

```yaml
- 1
- 2
- 3
- 4
- 5
```

```
Just [1,2,3,4,5]
```

## Plongée en profondeur

Il est également possible de traiter des données YAML plus complexes comme des objets et des tableaux imbriqués. Par exemple, supposons que vous ayez un fichier YAML contenant une liste d'utilisateurs avec leurs noms et leurs adresses e-mail. Vous pouvez utiliser `decode` pour le convertir en une liste d'objets Haskell avec les mêmes champs.

```yaml
- name: John
  email: john@example.com
- name: Kate
  email: kate@example.com
```

Voici un exemple de code pour cette situation:

```Haskell
import Data.YAML
import Data.Text (Text)

data User = User { name :: Text, email :: Text } deriving (Show)

instance FromYAML User where
  parseYAML = withMap "User" $ \m -> User <$> m .: "name" <*> m .: "email"

main = do
  let yml = "- name: John\n  email: john@example.com\n- name: Kate\n  email: kate@example.com"
  print yml
  print $ decode yml :: Maybe [User]
```

La sortie sera:

```yaml
- name: John
  email: john@example.com
- name: Kate
  email: kate@example.com
```

```
Just [ User { name = "John", email = "john@example.com" }, 
       User { name = "Kate", email = "kate@example.com" } ]
```

## Voir aussi

- [Documentation officielle de Data.YAML](https://hackage.haskell.org/package/yaml/docs/Data-YAML.html)
- [Exemple d'utilisation de YAML en Haskell](https://www.schoolofhaskell.com/user/geraldus/processing-yaml-files-41593)
- [Fichier YAML pour débutants](https://yaml.org/start.html)