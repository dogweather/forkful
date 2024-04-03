---
date: 2024-01-20 17:40:18.832847-07:00
description: "Cr\xE9er un fichier temporaire, c'est comme \xE9crire une note qu'on\
  \ jette apr\xE8s usage. Les programmeurs le font pour stocker des donn\xE9es de\
  \ mani\xE8re transitoire\u2026"
lastmod: '2024-03-13T22:44:57.854032-06:00'
model: gpt-4-1106-preview
summary: "Cr\xE9er un fichier temporaire, c'est comme \xE9crire une note qu'on jette\
  \ apr\xE8s usage."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## How to:
Haskell rend la gestion des fichiers temporaires élégante avec la bibliothèque `temporary`. Voici comment faire :

```haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hGetContents)

main :: IO ()
main = withSystemTempFile "monFichierTemp.txt" $ \path handle -> do
    -- Écrire dans le fichier temporaire
    hPutStrLn handle "Voici un exemple de texte temporaire."
  
    -- Repositionner le curseur au début pour lire depuis le début du fichier
    hSeek handle AbsoluteSeek 0 
  
    -- Lire le contenu et l'afficher
    contenu <- hGetContents handle
    putStrLn contenu
    -- Le fichier est supprimé automatiquement ici
```

Quand on exécute ce code, il affiche :
```
Voici un exemple de texte temporaire.
```

Le fichier existe pendant l'exécution et est supprimé automatiquement à la fin.

## Deep Dive
Historiquement en Haskell, on gérait les fichiers temporaires manuellement, mais c'était risqué et salissant. `System.IO.Temp` offre une abstraction pour les créer de manière sûre, qui garantit la suppression du fichier en fin d'utilisation. Il y a différentes fonctions selon les besoins : `withSystemTempFile`, `withTempFile`, `createTempDirectory`, et d'autres. Chaque fonction a ses spécificités. Par exemple, `withSystemTempFile` est idéale pour des fichiers dont on connaît le template de nom, et `createTempDirectory` pour un répertoire temporaire.

Alternativement, on pourrait utiliser `System.IO` pour un contrôle très fin, à l'ancienne. Mais pourquoi s'embêter quand `temporary` nous couvre ?

Enfin, derrière les rideaux, `temporary` utilise l'API de fichiers POSIX pour garantir la compatibilité et la sécurité.

## See Also
- La documentation Hackage de `temporary` : https://hackage.haskell.org/package/temporary
- Tutoriel de Haskell pour la manipulation de fichiers : https://www.haskell.org/tutorial/files.html
- Guide des bonnes pratiques en Haskell : https://wiki.haskell.org/Best_practices
