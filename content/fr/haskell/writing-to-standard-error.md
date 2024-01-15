---
title:                "Écriture de l'erreur standard"
html_title:           "Haskell: Écriture de l'erreur standard"
simple_title:         "Écriture de l'erreur standard"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire dans la sortie d'erreur standard peut être utile lorsque l'on souhaite afficher des messages d'erreur personnalisés ou des informations de débogage tout en exécutant un programme Haskell.

## Comment faire

Il suffit d'utiliser la fonction `hPutStrLn` en lui passant en paramètre l'index de la sortie d'erreur standard (`stderr`) ainsi que le message à afficher, comme ceci :

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Il y a quelque chose qui ne va pas !"
```

Cela affichera le message "Il y a quelque chose qui ne va pas !" dans la sortie d'erreur standard. Vous pouvez également utiliser `hGetContents` pour lire la sortie d'erreur standard dans une chaîne de caractères et la traiter ensuite.

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Il y a quelque chose qui ne va pas !"
  contents <- hGetContents stderr
  putStrLn ("Contenu de la sortie d'erreur standard : " ++ contents)
```

Lorsque vous exécutez ce code, vous obtiendrez une sortie semblable à ceci :

```
Contenu de la sortie d'erreur standard : Il y a quelque chose qui ne va pas !
```

## Plongée en profondeur

Il est important de noter que l'affichage dans la sortie d'erreur standard ne doit être utilisé que pour les messages d'erreur ou les informations de débogage. Pour les messages qui doivent être affichés normalement, il est préférable d'utiliser la sortie standard (`stdout`). Vous pouvez également utiliser `hPutStr` ou `hPutStrLn` pour afficher des messages dans d'autres fichiers de votre choix.

## Voir aussi

- [Documentation sur les fonctions `System.IO`](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [Guide pour débuter en Haskell](https://wiki.haskell.org/How_to_start_programming_in_Haskell)