---
title:    "Python: Trouver la longueur d'une chaîne de caractères"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante lors de la programmation en Python. Cela peut être utile pour manipuler des données, effectuer des opérations sur des chaînes de caractères ou simplement pour des fins de débogage.

## Comment faire

Il existe plusieurs façons de trouver la longueur d'une chaîne de caractères en Python. Voici deux exemples de code pour illustrer cela :

```Python
# Exemple 1 : Utiliser la fonction len()
chaine = "Bonjour"
longueur = len(chaine)
print(longueur) # Affiche 7
```

La fonction `len()` renvoie la longueur d'une chaîne de caractères en comptant le nombre de caractères inclus. Dans cet exemple, la chaîne "Bonjour" contient 7 caractères, donc la fonction `len()` renvoie 7.

```Python
# Exemple 2 : Utiliser une boucle
chaine = "Hello World"
longueur = 0
for lettre in chaine:
    longueur += 1
print(longueur) # Affiche 11
```

Dans cet exemple, une boucle est utilisée pour parcourir chaque caractère de la chaîne et incrémenter une variable `longueur` à chaque itération. À la fin de la boucle, la variable `longueur` contient la longueur de la chaîne.

## Plongée en profondeur

Il est important de noter que la fonction `len()` renvoie le nombre de caractères inclus dans une chaîne de caractères, et non le nombre de mots ou le nombre d'espaces. Par exemple, la chaîne "Hello World" contient 11 caractères, mais seulement 2 mots et un espace entre eux.

De plus, la fonction `len()` peut être utilisée pour trouver la longueur de toutes sortes de données en Python, pas seulement les chaînes de caractères. Par exemple, elle peut être utilisée pour trouver le nombre d'éléments dans une liste ou le nombre de clés dans un dictionnaire.

## Voir aussi

- Documentation officielle de Python sur la fonction `len()` : https://docs.python.org/fr/3/library/functions.html#len
- Tutoriel sur les chaînes de caractères en Python : https://python.developpez.com/cours/TutoApprendrePython3/#LIII-B
- Autres méthodes pour trouver la longueur d'une chaîne de caractères en Python : https://www.geeksforgeeks.org/python-string-length-len/