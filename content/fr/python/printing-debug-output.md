---
title:    "Python: Imprimer des sorties de débogage"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi quelqu'un serait-il intéressé par l'affichage de résultats de débogage ? Lorsque nous programmons en Python, il y a parfois des erreurs qui se produisent et la seule façon de les résoudre est de comprendre ce qui se passe dans notre code. C'est là que l'affichage de résultats de débogage entre en jeu. En affichant à l'écran des informations sur les variables, les valeurs et les actions du programme, nous pouvons facilement repérer où se situe le problème et le corriger rapidement.

## Comment faire

Pour afficher des résultats de débogage en Python, il existe deux options principales : la fonction `print()` et le module `logging`.

```Python
# Exemple utilisant la fonction print()
age = 25
print("Mon âge est :", age)

# Output : Mon âge est : 25

# Exemple utilisant le module logging
import logging

logging.basicConfig(level=logging.DEBUG)
age = 25
logging.debug("Mon âge est : %s", age)

# Output : DEBUG:root:Mon âge est : 25
```

Dans les deux cas, nous obtenons le même résultat affiché à l'écran, mais le module `logging` offre des fonctionnalités plus avancées telles que la gestion des niveaux de débogage et l'enregistrement des résultats dans un fichier.

## Plongée en profondeur

L'affichage de résultats de débogage peut être très utile, mais il est important de ne pas en abuser. Trop d'affichages peuvent rendre le programme difficile à lire et à comprendre. Il est donc important de les utiliser judicieusement et de les supprimer une fois que le problème est résolu.

De plus, le module `logging` offre des fonctionnalités de formatage pour personnaliser l'affichage des résultats de débogage. On peut également utiliser des paramètres tels que `exc_info=True` pour afficher des informations sur les erreurs et les exceptions.

## Voir aussi

- [Documentation officielle de Python sur le débogage](https://docs.python.org/fr/3/library/logging.html)
- [Guide du débogage en Python sur RealPython](https://realpython.com/python-debugging-pdb/)

Merci d'avoir lu cet article sur l'affichage de résultats de débogage en Python. J'espère qu'il vous a été utile dans votre parcours de programmation !