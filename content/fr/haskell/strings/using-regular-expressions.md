---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:48.940000-07:00
description: "Comment : En Haskell, les fonctionnalit\xE9s regex ne font pas partie\
  \ de la biblioth\xE8que standard, n\xE9cessitant l'utilisation de paquets tiers\
  \ comme `regex-\u2026"
lastmod: '2024-03-13T22:44:57.823236-06:00'
model: gpt-4-0125-preview
summary: "En Haskell, les fonctionnalit\xE9s regex ne font pas partie de la biblioth\xE8\
  que standard, n\xE9cessitant l'utilisation de paquets tiers comme `regex-base` accompagn\xE9\
  \ d'un backend compatible tel que `regex-posix` (pour le support des regex POSIX),\
  \ `regex-pcre` (pour les regex compatibles Perl), etc."
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

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
