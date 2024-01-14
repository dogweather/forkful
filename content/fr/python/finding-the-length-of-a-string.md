---
title:                "Python: Trouver la longueur d'une chaîne de caractères"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Le calcul de la longueur d'une chaîne de caractères est un concept fondamental en programmation. Cela vous permet de déterminer le nombre de caractères présents dans une chaîne, ce qui peut être utile dans de nombreuses situations, telles que la validation de la saisie utilisateur ou la manipulation de données textuelles.

## Comment faire
Pour trouver la longueur d'une chaîne en Python, vous pouvez utiliser la fonction intégrée `len()`. Elle prend la chaîne en tant que paramètre et renvoie le nombre de caractères.

```Python
# Exemple d'utilisation de la fonction len()
texte = "Bonjour tout le monde!"
print(len(texte))
```

###### Output:
18

Lorsque vous exécutez ce code, vous devriez voir le nombre `18` s'afficher dans la console, ce qui correspond à la longueur de la chaîne "Bonjour tout le monde!".

## Plongée en profondeur
En Python, une chaîne de caractères est considérée comme une séquence, ce qui signifie que vous pouvez également utiliser l'indexation pour accéder à des parties spécifiques de la chaîne. La fonction `len()` vous permet de connaître le nombre total d'éléments dans la séquence, y compris les caractères, les espaces et les symboles de ponctuation.

Il est important de noter que la fonction `len()` ne compte pas les caractères de début et de fin, qui sont représentés par des guillemets simples ou doubles, dans la longueur totale de la chaîne.

## Voir aussi
- [Documentation officielle de Python sur la fonction len()](https://docs.python.org/fr/3/library/functions.html#len)
- [Tutorialspoint - Python String Length](https://www.tutorialspoint.com/python/string_len.htm)
- [GeeksforGeeks - Python | Length of a String](https://www.geeksforgeeks.org/python-length-of-a-string/)