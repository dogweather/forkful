---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'impression des sorties de débogage en Python est l'art d'utiliser la fonction `print()` pour afficher des informations supplémentaires pendant l'exécution du code. Les programmeurs l'utilisent souvent pour comprendre le comportement du programme, en particulier lorsqu'ils traitent des bugs complexes.

## Comment faire: 
Maintenant, voyons comment on peut l'utiliser dans notre code Python. Voici un exemple simple.

```Python
def divide(x, y):  
    debug_output = f"Dividing {x} by {y}"
    print(debug_output)
    return x / y

print(divide(10, 2))
```
La sortie de ce code sera :

```Python
Dividing 10 by 2
5.0
```

Comme vous pouvez le constater, l'information de débogage aide à comprendre ce que le code fait à ce moment précis.

## Deep Dive

Historiquement, l'utilisation de `print()` pour le débogage a ses racines dans les premiers jours de la programmation. Avec l'absence d'outils de débogage sophistiqués, les programmeurs ont dû se reposer sur le bon vieux `print(f)`. 

Cependant, en Python, vous avez d'autres façons de déboguer votre code. L'une des méthodes les plus populaires consiste à utiliser le module `logging` de Python. Ce module offre plus de flexibilité que la fixe `print()` en permettant de contrôler le niveau de détail des messages enregistrés, de les formater de manière attrayante et d'écrire les messages de débogage dans des fichiers pour une consultation ultérieure.

Voici un exemple utilisant le module `logging` :

```Python
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

def divide(x, y):
    debug_output = f"Dividing {x} by {y}"
    logger.debug(debug_output)
    return x / y

print(divide(10, 2))
```

Dans le monde de la programmation moderne, l'usage de `print()` pour le débogage peut apparaître comme peu professionnel et dépassé. Mais ne vous y trompez pas - c'est un outil simple mais puissant qui peut vous aider à rapidement déboguer et comprendre le flux de code.

## Voir Aussi

1. Logging in Python (https://docs.python.org/3/library/logging.html)
2. Debugging in Python (https://realpython.com/python-debugging-pdb/)
3. Debugging techniques in Python (https://thepythonguru.com/debugging-techniques-in-python/)