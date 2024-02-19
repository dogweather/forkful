---
aliases:
- /fr/haskell/reading-a-text-file/
date: 2024-01-20 17:54:24.557245-07:00
description: "Lire un fichier texte, c'est r\xE9cup\xE9rer son contenu pour l'utiliser\
  \ dans notre programme. Les programmeurs le font pour manipuler des donn\xE9es,\
  \ configurer\u2026"
lastmod: 2024-02-18 23:09:08.894596
model: gpt-4-1106-preview
summary: "Lire un fichier texte, c'est r\xE9cup\xE9rer son contenu pour l'utiliser\
  \ dans notre programme. Les programmeurs le font pour manipuler des donn\xE9es,\
  \ configurer\u2026"
title: Lecture d'un fichier texte
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Lire un fichier texte, c'est récupérer son contenu pour l'utiliser dans notre programme. Les programmeurs le font pour manipuler des données, configurer des systèmes ou charger des ressources.

## How to: (Comment faire :)
Haskell rend la lecture de fichiers texte simple avec quelques fonctions clés. Voici le standard :

```Haskell
import System.IO

main :: IO ()
main = do
    content <- readFile "chemin/vers/le/fichier.txt"
    putStrLn content
```

Si votre fichier est `hello.txt` et contient "Bonjour, monde!", l'exécution donnerait :

```
Bonjour, monde!
```

## Deep Dive (Plongée en profondeur)
Historiquement, Haskell gère les fichiers de manière paresseuse avec `readFile`, chargement du contenu à la demande. Ce n'est pas toujours idéal, surtout pour les gros fichiers ou les exigences en temps réel. On peut aussi lire avec `getContents` et `hGetContents` pour plus de contrôle.

Alternativement, vous pouvez utiliser `Data.ByteString` pour la lecture de blocs binaires ou `Text` de `Data.Text` pour un traitement plus efficace des chaînes.

En coulisse, `readFile` utilise `openFile` et `hGetContents`. Haskell gère la fermeture automatique des fichiers, mais on peut utiliser `withFile` si on veut plus de contrôle.

## See Also (Voir également)
- La documentation sur [hackage:base System.IO](https://hackage.haskell.org/package/base-4.16.1.0/docs/System-IO.html)
- Pour un aperçu plus avancé, consultez les paquets `bytestring` et `text` sur Hackage.
