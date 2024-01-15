---
title:                "Capitalisation d'une chaîne de caractères."
html_title:           "Python: Capitalisation d'une chaîne de caractères."
simple_title:         "Capitalisation d'une chaîne de caractères."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être remarqué que certaines chaînes de caractères sont écrites en majuscules et d'autres en minuscules. Parfois, vous pourriez avoir besoin de modifier la casse d'une chaîne pour des raisons de lisibilité ou de conformité aux normes. Heureusement, Python dispose d'une méthode simple pour effectuer cette tâche : la fonction capitalize().

## Comment faire

Pour utiliser la fonction capitalize(), vous devez tout d'abord définir votre chaîne de caractères. Ensuite, vous pouvez appeler la fonction en utilisant la syntaxe suivante :

```Python
my_string = "ceci est un exemple"
print(my_string.capitalize())
```

La sortie sera "Ceci est un exemple". Comme vous pouvez le voir, la première lettre de la chaîne a été mise en majuscule.

## Plongée en profondeur

En plus de la fonction capitalize(), Python dispose également d'autres fonctions pour gérer la casse des chaînes. Vous pouvez utiliser la méthode upper() pour mettre toute une chaîne en majuscules, ou la méthode lower() pour mettre toute une chaîne en minuscules.

Il est également important de noter que la fonction capitalize() ne change que la première lettre de la chaîne. Si vous souhaitez mettre en majuscule toutes les premières lettres de chaque mot dans une chaîne, vous pouvez utiliser la méthode title().

## Voir aussi

- [Documentation Python sur la fonction capitalize()](https://docs.python.org/fr/3/library/stdtypes.html#str.capitalize)
- [Tutorial sur les manipulations de chaînes en Python](https://realpython.com/python-strings/)