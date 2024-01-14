---
title:                "Haskell: Écrire vers les erreurs standard"
simple_title:         "Écrire vers les erreurs standard"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Êtes-vous frustré lorsqu'un programme plante et que vous ne savez pas pourquoi? En utilisant la fonction `hPutStrLn stderr`, vous pouvez facilement écrire des messages d'erreur pour aider à déboguer votre code Haskell.

## Comment faire

Utiliser `hPutStrLn stderr` pour écrire un message sur la sortie standard d'erreur est simple. Voici un exemple de code pour illustrer son utilisation:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Ce message s'affichera sur la sortie standard d'erreur"
```

Lorsque vous exécutez ce code, le message sera affiché sur la sortie standard d'erreur plutôt que sur la sortie standard normale. Cela permet de distinguer les erreurs des autres sorties de votre programme.

## Plongée en profondeur

La fonction `hPutStrLn` appartient au module `System.IO`, qui fournit des fonctions pour les entrées/sorties dans Haskell. Elle prend deux paramètres: le premier est le fichier sur lequel écrire (ici, `stderr` pour la sortie standard d'erreur) et le second est le message à écrire.

Il est important de noter que `stderr` est un flux non-tamponné, ce qui signifie que les messages seront affichés immédiatement, sans attente de la fin de l'exécution du programme. Cela peut être utile pour déboguer les problèmes qui se produisent pendant l'exécution du code.

## Voir aussi

- [Documentation officielle de `System.IO`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)
- [Tutoriel sur la gestion des erreurs en Haskell](https://www.vex.net/~trebla/haskell/error_handling.xhtml)
- [Didacticiel interactif pour apprendre Haskell](https://learnyouahaskell.com/)