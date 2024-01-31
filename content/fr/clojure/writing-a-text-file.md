---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
simple_title:         "Écriture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire un fichier texte, c'est sauvegarder des données lisibles dans un fichier sur le disque. Les programmeurs le font pour persister de l'information, comme des logs, des configurations ou des données à partager.

## How to:
Clojure utilise `spit` pour écrire dans un fichier et `slurp` pour lire. Voici comment écrire du texte :

```Clojure
(spit "chemin/vers/le_fichier.txt" "Salut, c'est un test!")
```

Et pour lire le fichier que vous venez d'écrire :

```Clojure
(println (slurp "chemin/vers/le_fichier.txt"))
```

Ça affichera `Salut, c'est un test!`.

## Deep Dive
Avant, on utilisait Java pour écrire des fichiers en Clojure. Maintenant, avec `spit`, c'est plus simple. Mais si vous voulez écrire pièce par pièce, regardez `with-open` et `writer`. En dessous, `spit` et `slurp` utilisent les mêmes mécanismes de streams que Java.

## See Also
- La documentation officielle de Clojure : [clojure.org](https://clojure.org/)
- Tutoriels pour débuter : [Clojure for the Brave and True](https://www.braveclojure.com/)
