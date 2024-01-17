---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Python: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

Trouver la longueur d'une chaîne de caractères est une tâche courante pour les programmeurs en Python. Cela leur permet de connaître le nombre exact de caractères dans une chaîne et de les manipuler en conséquence.

# Comment faire:

Pour trouver la longueur d'une chaîne en Python, vous pouvez utiliser la fonction intégrée "len()". Voici un exemple de code et sa sortie:

```Python
# Exemple de code
chaine = "Bonjour!"
longueur = len(chaine)
print(longueur)
```
```Python
# Sortie
8
```

# Baignade en profondeur:

Histoire: Trouver la longueur d'une chaîne en programmation était une tâche laborieuse avant que la fonction "len()" ne soit introduite dans le langage de programmation Python.

Alternatives: En plus d'utiliser la fonction "len()", vous pouvez également utiliser une boucle pour parcourir la chaîne et compter le nombre de caractères un par un. Cependant, cela peut être plus complexe et prendre plus de temps.

Détails de mise en œuvre: La fonction "len()" accepte un seul argument, qui doit être une chaîne de caractères ou une séquence telle qu'une liste ou un tuple. Elle renvoie ensuite le nombre de caractères dans cette séquence.

# Voir aussi:

Vous pouvez en apprendre plus sur les fonctions intégrées en Python en consultant la documentation officielle: https://docs.python.org/3/library/functions.html#built-in-functions