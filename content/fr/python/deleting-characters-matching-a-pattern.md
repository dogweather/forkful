---
title:                "Suppression de caractères correspondants à un motif"
html_title:           "Python: Suppression de caractères correspondants à un motif"
simple_title:         "Suppression de caractères correspondants à un motif"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi quelqu'un voudrait supprimer des caractères correspondant à un modèle dans un programme Python. Eh bien, cela peut être utile dans de nombreuses situations, comme nettoyer les données, filtrer les entrées utilisateur ou traiter des chaînes de caractères spécifiques.

## Comment faire

Pour supprimer des caractères correspondant à un modèle dans une chaîne de caractères, vous pouvez utiliser la méthode `sub()` de l'objet `re` (pour "regular expression") de Python. Cette méthode prend deux arguments : le modèle que vous souhaitez supprimer et la chaîne de caractères sur laquelle vous souhaitez appliquer la suppression. Voici un exemple de code :

```Python
import re

input_string = "Cette phrase contient des nombres comme 123 et des caractères spéciaux !@#$"
output_string = re.sub('[0-9!@#$]', '', input_string)
print(output_string)
```

Résultat :

```
Cette phrase contient des nombres comme et des caractères spéciaux
```

Comme vous pouvez le voir, la méthode `sub()` a supprimé tous les caractères correspondant au modèle `[0-9!@#$]` (chiffres de 0 à 9 et les caractères !, @, # et $) dans la chaîne de caractères `input_string`. Il est également possible d'utiliser des modèles plus complexes, tels que `[a-z]` pour supprimer toutes les lettres minuscules ou `[^A-Za-z]` pour supprimer tous les caractères non alphabétiques.

## Plongée en profondeur

Comme mentionné précédemment, la méthode `sub()` utilise des expressions régulières pour rechercher et supprimer des caractères correspondant à un modèle. Les expressions régulières sont un langage de programmation à part entière, mais elles peuvent être très utiles lorsqu'il s'agit de manipuler des chaînes de caractères complexes. Voici quelques-uns des modèles les plus couramment utilisés :

* `[0-9]` : supprime tous les chiffres de 0 à 9.
* `[a-z]` : supprime toutes les lettres minuscules.
* `[A-Z]` : supprime toutes les lettres majuscules.
* `[^0-9]` : supprime tous les caractères non numériques.
* `[^A-Za-z]` : supprime tous les caractères non alphabétiques.

Il est également possible d'inclure plusieurs modèles dans un seul argument `sub()`, par exemple `'[0-9A-Z]'` pour supprimer les chiffres et les lettres majuscules.

## Voir aussi

* [Python Regular Expressions Tutorial](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial) : un tutoriel complet sur l'utilisation des expressions régulières en Python.
* [Documentation officielle de Python sur les expressions régulières](https://docs.python.org/fr/3/library/re.html) : informations détaillées sur toutes les fonctionnalités liées aux expressions régulières en Python.