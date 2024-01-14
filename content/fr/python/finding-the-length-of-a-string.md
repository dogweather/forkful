---
title:                "Python: Trouver la longueur d'une chaîne de caractères."
simple_title:         "Trouver la longueur d'une chaîne de caractères."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent utile de connaître la longueur d'une chaîne de caractères lorsque l'on programme en Python. Cela peut être nécessaire pour effectuer certains calculs, valider des données ou simplement afficher des informations à l'utilisateur. Dans cet article, nous allons vous montrer comment trouver la longueur d'une chaîne en Python et approfondir un peu le sujet pour vous donner une meilleure compréhension de son fonctionnement.

## Comment faire

Pour trouver la longueur d'une chaîne en Python, nous allons utiliser la fonction intégrée `len()`. Voici un exemple de code avec une chaîne de caractères et son résultat :

```python
nom = "Marie"
print(len(nom))
```

Output : `5`

Comme vous pouvez le voir, la fonction `len()` nous renvoie le nombre de caractères dans la chaîne donnée. Voici un autre exemple avec une chaîne de caractères contenant des nombres et des lettres :

```python
informations = "âge: 28, taille: 175cm"
print(len(informations))
```

Output : `27`

Attention, la fonction `len()` ne compte pas les espaces vides à la fin d'une chaîne. Par exemple :

```python
greeting = "Bonjour!"
print(len(greeting))
```

Output : `8` car il y a un espace vide à la fin de la chaîne.

## Plongée en profondeur

Maintenant que vous savez comment trouver la longueur d'une chaîne en utilisant la fonction `len()`, il est important de comprendre comment cela fonctionne réellement. En Python, les chaînes de caractères sont des objets de type `str` et la fonction `len()` appelle la méthode `__len__()` de cet objet. Cette méthode retourne le nombre de caractères dans la chaîne et c'est pourquoi nous obtenons le résultat attendu.

Une chose importante à noter est que la longueur d'une chaîne peut varier en fonction de l'encodage utilisé. Par exemple, la lettre "é" sera comptée comme un seul caractère en encodage UTF-8, mais comme deux caractères en encodage latin-1.

## Voir aussi

- [Documentation officielle de Python sur la fonction `len()`](https://docs.python.org/fr/3/library/functions.html#len)
- [W3Schools sur la fonction `len()` en Python](https://www.w3schools.com/python/ref_func_len.asp)
- [Cette vidéo YouTube pour une explication visuelle de la longueur des chaînes en Python (en anglais)](https://www.youtube.com/watch?v=VORpmJNgDmw)