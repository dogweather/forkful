---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire un fichier texte permet de sauvegarder des données persistantes. Les développeurs font ça pour des logs, des configurations, ou des sauvegardes de l'état d'une application.

## How to:
On peut écrire dans un fichier avec `writeFile`:

```Haskell
import System.IO

main :: IO ()
main = do
    let content = "Salut, ceci est un fichier texte!"
    writeFile "exemple.txt" content
```

Sortie: Un fichier `exemple.txt` est créé avec le contenu spécifié.

Utiliser `appendFile` pour ajouter du contenu:

```Haskell
main :: IO ()
main = do
    let moreContent = "\nAjout au fichier existant."
    appendFile "exemple.txt" moreContent
```

Sortie: "Ajout au fichier existant." est ajouté à la fin du fichier `exemple.txt`.

## Deep Dive:
Haskell existait depuis les années 1980, mais IO avec `writeFile` et `appendFile` n'est devenu élégant qu'avec l'introduction des monades IO. Parmi les alternatives, il y a `hPutStr` pour plus de contrôle. Ces fonctions cachent la complexité de la gestion des fichiers.

## See Also:
- Haskell IO Tutorial: https://www.haskell.org/tutorial/io.html
- Documentation pour `System.IO`: http://hackage.haskell.org/package/base/docs/System-IO.html
- Blog sur les monades IO: https://wiki.haskell.org/IO_inside
