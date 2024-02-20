---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:31.811991-07:00
description: "YAML, abr\xE9viation de \"YAML Ain't Markup Language\" (YAML n'est pas\
  \ un langage de balisage), est une norme de s\xE9rialisation de donn\xE9es conviviale\
  \ pour les\u2026"
lastmod: 2024-02-19 22:05:16.592150
model: gpt-4-0125-preview
summary: "YAML, abr\xE9viation de \"YAML Ain't Markup Language\" (YAML n'est pas un\
  \ langage de balisage), est une norme de s\xE9rialisation de donn\xE9es conviviale\
  \ pour les\u2026"
title: Travailler avec YAML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

YAML, abréviation de "YAML Ain't Markup Language" (YAML n'est pas un langage de balisage), est une norme de sérialisation de données conviviale pour les humains, qui peut être utilisée pour tous les langages de programmation. Les programmeurs utilisent souvent YAML dans les fichiers de configuration et pour l'échange de données entre langages en raison de sa lisibilité et de sa structure simple.

## Comment :

Haskell n'a pas de support intégré pour le traitement de YAML, mais vous pouvez utiliser des bibliothèques tierces comme `yaml` et `aeson` pour analyser et générer des données YAML. Voici comment vous pouvez commencer :

### Lire du YAML
D'abord, ajoutez le paquet `yaml` aux dépendances de votre projet. Ensuite, vous pouvez utiliser l'exemple suivant pour analyser un document YAML simple :

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- Exemple de données YAML
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- Définir une structure de données qui correspond au document YAML
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "Erreur lors de l'analyse du YAML : " ++ show err
    Right person -> print person
```
Un exemple de sortie pour le code ci-dessus pourrait ressembler à :
```
Person {name = "John Doe", age = 30}
```

### Écrire du YAML
Pour générer du YAML à partir de structures de données Haskell, vous pouvez utiliser les fonctionnalités d'encodage du paquet `yaml` comme montré ci-dessous :

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- Utilisation de la structure de données Person de l'exemple précédent

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
La sortie de ce programme sera une chaîne de caractères formatée en YAML :
```
name: Jane Doe
age: 25
```

Ces exemples devraient servir de point de départ pour travailler avec YAML en Haskell. Selon vos besoins, vous voudrez peut-être explorer des fonctionnalités et options plus avancées fournies par ces bibliothèques.
