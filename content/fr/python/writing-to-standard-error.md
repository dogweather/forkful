---
title:    "Python: Écrire vers l'erreur standard"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire dans la sortie d'erreur standard peut sembler intimidant pour de nombreux programmeurs débutants, mais c'est une compétence importante à maîtriser. Ce processus vous permet d'afficher des messages d'erreur et de débogage dans votre code, ce qui peut vous aider à mieux comprendre et résoudre les problèmes dans vos programmes.

## Comment faire

Voici un exemple simple qui montre comment écrire dans la sortie d'erreur standard en utilisant le module `sys` en Python :

```Python
import sys

sys.stderr.write("Erreur: la valeur entrée n'est pas un nombre entier")
```

Lorsque vous exécutez ce code, vous remarquerez que le message d'erreur est affiché en rouge, séparé du reste de votre programme. Cela permet de le distinguer des autres sorties et de le rendre plus visible pour le débogage.

Vous pouvez également utiliser la méthode `.flush()` pour forcer l'affichage immédiat des messages dans la sortie d'erreur standard, plutôt que d'attendre la fin de l'exécution du programme.

## Plongée en profondeur

Lorsque vous écrivez dans la sortie d'erreur standard, il est important de noter que cela ne garantit pas que le programme s'arrêtera. En effet, les erreurs peuvent être ignorées ou gérées par des blocs `try/except` dans votre code.

De plus, il est important de toujours fermer la sortie d'erreur standard `sys.stderr` après l'avoir utilisée, en utilisant la méthode `.close()`.

## Voir aussi

- [Documentation officielle Python - Manipulation de la sortie d'erreur standard](https://docs.python.org/fr/3/library/sys.html#sys.stderr)
- [Article sur la gestion des erreurs en Python](https://realpython.com/python-exceptions/)
- [Guide de débogage en Python](https://www.fullstackpython.com/debugging.html)