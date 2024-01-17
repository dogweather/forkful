---
title:                "Écrire sur la sortie standard"
html_title:           "Python: Écrire sur la sortie standard"
simple_title:         "Écrire sur la sortie standard"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Écrire vers la sortie d'erreur standard (stderr) est une pratique courante en programmation qui consiste à rediriger les messages d'erreur vers un canal spécifique plutôt que de les intégrer aux messages de sortie standards (stdout). Cela permet aux développeurs de mieux distinguer les informations de débogage des résultats attendus, et d'améliorer ainsi la lisibilité du code.

## Comment faire:

Utiliser la fonction `sys.stderr.write(message)` pour écrire vers la sortie d'erreur standard (stderr), en donnant en paramètre le message à afficher. Ensuite, n'oubliez pas d'utiliser l'instruction `import sys` pour importer le module `sys` qui contient cette fonction.

```
import sys

sys.stderr.write("Une erreur est survenue.")
```

Cela affichera "Une erreur est survenue." dans la sortie d'erreur standard (stderr).

## Plongée en profondeur:

Avant l'émergence de la console d'erreur standard (stderr), les messages d'erreurs étaient souvent mélangés avec les messages de sortie standards (stdout), rendant ainsi la tâche de débogage plus difficile pour les programmeurs. C'est pourquoi l'utilisation de la sortie d'erreur standard (stderr) devient de plus en plus courante dans le développement logiciel actuel.

Il existe également d'autres façons d'écrire vers la sortie d'erreur, telles que la fonction `logging.error(message)` du module `logging`. Cependant, cette méthode nécessite la configuration d'un logger et peut être plus lourde à mettre en place par rapport à l'utilisation de la fonction `sys.stderr.write`.

En termes d'implémentation, la sortie d'erreur standard (stderr) est généralement gérée par le système d'exploitation plutôt que par le langage de programmation lui-même. Cela signifie que la redirection vers la sortie d'erreur standard (stderr) peut varier selon le système d'exploitation utilisé.

## Voir aussi:

- Documentation officielle de Python sur la sortie d'erreur standard (https://docs.python.org/fr/3/library/sys.html#sys.stderr)
- Article sur la gestion des erreurs en Python (https://realpython.com/python-logging/)