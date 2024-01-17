---
title:                "Afficher la sortie de débogage"
html_title:           "Python: Afficher la sortie de débogage"
simple_title:         "Afficher la sortie de débogage"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi faire?

Imprimer les sorties de débogage est le fait de mettre des messages de débogage dans votre code pour comprendre son comportement pendant l'exécution. Cela aide les programmeurs à identifier et résoudre les erreurs dans leur code.

# Comment faire:

Voici un exemple de code Python utilisant la fonction `print()` pour afficher des messages de débogage:

```
num1 = 5
num2 = 10

# Afficher les valeurs des variables
print("Valeur de num1:", num1)
print("Valeur de num2:", num2)

# Effectuer une opération et afficher le résultat
print(num1 + num2)
```

Sortie:

```
Valeur de num1: 5
Valeur de num2: 10
15
```

# Plongée en profondeur:

Le débogage est une partie essentielle du processus de développement de logiciels depuis les premiers jours de la programmation informatique. Avant les avancées technologiques telles que les débogueurs intégrés, les programmeurs utilisaient principalement l'impression de messages pour comprendre et résoudre les erreurs dans leur code.

En dehors de l'utilisation de la fonction `print()`, il existe d'autres techniques de débogage, telles que l'utilisation de débogueurs intégrés, la mise en place de points d'arrêt ou l'utilisation de journaux pour enregistrer des informations de débogage. Cependant, l'impression de messages reste l'une des méthodes les plus simples et les plus utilisées pour le débogage en Python.

# Voir aussi:

Pour en savoir plus sur l'impression de débogage en Python, vous pouvez consulter les ressources suivantes:

- [Documentation Python sur la fonction print()](https://docs.python.org/fr/3/library/functions.html#print)
- [Article sur le débogage en Python](https://realpython.com/python-debugging-pdb/) sur Real Python (en anglais)