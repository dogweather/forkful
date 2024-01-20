---
title:                "L'écriture sur l'erreur standard"
html_title:           "Haskell: L'écriture sur l'erreur standard"
simple_title:         "L'écriture sur l'erreur standard"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Ecrire vers la sortie d'erreur standard (stderr) est une pratique courante pour les programmeurs en Haskell. Cela permet d'afficher des messages d'erreur et de débogage, ainsi que de mieux gérer les exceptions. Cela peut également améliorer la lisibilité du code en séparant les messages d'erreur du reste de la sortie du programme. 

## Comment:

Voici un exemple simple d'utilisation de la sortie d'erreur standard en Haskell:

```Haskell
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    hPutStrLn stderr "Ceci est un message d'erreur"
    putStrLn "Ceci est le reste de la sortie du programme"
```

La sortie de ce programme sera:

```
Ceci est le reste de la sortie du programme
Ceci est un message d'erreur
```

Remarquez comment le message d'erreur apparaît après la sortie du programme. Cela peut être utile pour identifier où et pourquoi une erreur s'est produite. 

## Plongée en Profondeur:

Initialement, la sortie d'erreur standard était utilisée uniquement pour les messages d'erreur, mais elle est également devenue un moyen populaire pour les messages de débogage et les rapports d'exception. Cela est dû en partie à la popularité des systèmes de journalisation tels que "log4j" en Java. 

Des alternatives à l'écriture vers la sortie d'erreur standard incluent l'utilisation de bibliothèques de journalisation telles que "log" ou la création de votre propre fonction pour gérer les messages de débogage. Ces alternatives peuvent fournir une meilleure gestion et organisation des messages d'erreur et de débogage. 

Pour implémenter l'écriture vers la sortie d'erreur standard en Haskell, il suffit d'utiliser la fonction "hPutStrLn" du module "System.IO". Cette fonction prend deux arguments : le flux de sortie, dans ce cas "stderr", et la chaîne de caractères à écrire.

## Voir Aussi:

- [Documentation officielle de Haskell](https://www.haskell.org/)
- [Module System.IO](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)