---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:17.804092-07:00
description: "\xC9crire dans l'erreur standard (stderr) en Haskell permet aux programmes\
  \ de diff\xE9rencier leur sortie entre les r\xE9sultats normaux et les messages\
  \ d'erreur.\u2026"
lastmod: '2024-03-11T00:14:31.799848-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire dans l'erreur standard (stderr) en Haskell permet aux programmes\
  \ de diff\xE9rencier leur sortie entre les r\xE9sultats normaux et les messages\
  \ d'erreur.\u2026"
title: "\xC9crire sur l'erreur standard"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire dans l'erreur standard (stderr) en Haskell permet aux programmes de différencier leur sortie entre les résultats normaux et les messages d'erreur. Cela est crucial pour signaler des problèmes et pour le débogage, sans encombrer la sortie standard (stdout) qui porte souvent les données principales ou le résultat du programme.

## Comment faire :
En Haskell, écrire dans stderr est simple avec le module `System.IO` de la bibliothèque de base. Voici un exemple basique pour démontrer :

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Ceci est un message d'erreur."
```

La sortie de ce programme vers stderr serait :

```
Ceci est un message d'erreur.
```

Si vous travaillez dans une application plus complexe, ou si vous avez besoin d'un meilleur contrôle sur la journalisation (y compris des erreurs), vous pourriez opter pour une bibliothèque tierce. Un choix populaire est `monad-logger` qui s'intègre au style de programmation `mtl` de Haskell. Voici un petit extrait utilisant `monad-logger` :

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Ceci est un message d'erreur en utilisant monad-logger."
```

Lors de l'exécution, la version `monad-logger` produit également un message d'erreur, mais il est équipé de plus de contexte comme des horodatages ou des niveaux de journalisation, selon la configuration :

```
[Erreur] Ceci est un message d'erreur en utilisant monad-logger.
```

Les deux méthodes servent à écrire dans stderr, le choix dépendant largement de la complexité et des besoins de votre application.
