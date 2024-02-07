---
title:                "Utilisation des expressions régulières"
date:                  2024-02-03T19:16:48.940000-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des expressions régulières"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières en programmation sont des séquences de caractères qui définissent un motif de recherche, généralement employé pour la recherche et la manipulation de chaînes de caractères. Les programmeurs Haskell utilisent les expressions régulières pour des tâches allant de la simple correspondance de chaînes à des traitements de texte complexes, profitant de leur efficacité et de leur polyvalence dans le traitement des données textuelles.

## Comment :
En Haskell, les fonctionnalités regex ne font pas partie de la bibliothèque standard, nécessitant l'utilisation de paquets tiers comme `regex-base` accompagné d'un backend compatible tel que `regex-posix` (pour le support des regex POSIX), `regex-pcre` (pour les regex compatibles Perl), etc. Voici comment vous pouvez utiliser ces paquets pour travailler avec les expressions régulières.

D'abord, assurez-vous d'avoir les paquets installés en ajoutant `regex-posix` ou `regex-pcre` au fichier `.cabal` de votre projet ou en installant via cabal directement :

```bash
cabal install regex-posix
```
ou
```bash
cabal install regex-pcre
```

### Utilisation de `regex-posix` :

```haskell
import Text.Regex.Posix ((=~))

-- Vérifier si une chaîne correspond à un motif
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- Trouver la première correspondance
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- Sortie: True
    print $ findFirst "bonjour, bonne nuit" "bon"
    -- Sortie: "bon"
```

### Utilisation de `regex-pcre` :

```haskell
import Text.Regex.PCRE ((=~))

-- Trouver toutes les correspondances
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Sortie: ["test1","test2","test3"]
```

Chaque bibliothèque a ses particularités, mais la méthodologie générale d'utilisation de `=~` pour appliquer la regex reste constante, que ce soit pour vérifier une correspondance ou extraire des sous-chaînes. Le choix entre `regex-posix` ou `regex-pcre` dépend largement des besoins de votre projet et des capacités regex spécifiques requises.
