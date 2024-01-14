---
title:                "Python: Capitaliser une chaîne de caractères"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

Vous vous demandez peut-être pourquoi il est important de mettre des majuscules dans une chaîne de caractères. Tout simplement pour des raisons d'esthétique et de lisibilité. Les majuscules permettent de distinguer clairement les différents mots dans une phrase et rendent le texte plus agréable à lire.

## Comment faire

Si vous utilisez Python, il existe une méthode très simple pour mettre des majuscules dans une chaîne de caractères. Il suffit d'utiliser la fonction `upper()`. Voici un exemple de code qui vous montre comment l'utiliser:

```Python
ma_chaine = "python est génial"
print(ma_chaine.upper())
```

Cela produira la sortie suivante: `PYTHON EST GÉNIAL`

## Plongée en profondeur

Maintenant que vous savez comment utiliser la fonction `upper()` pour mettre des majuscules dans vos chaînes de caractères, voici quelques informations supplémentaires pour vous aider à mieux comprendre. La méthode `upper()` est en fait une méthode de la classe `str` qui appartient à la bibliothèque standard de Python. Elle prend en paramètre une chaîne de caractères et renvoie une nouvelle chaîne avec toutes les lettres en majuscule.

Il est également intéressant de noter qu'il existe une autre méthode appelée `capitalize()` qui met uniquement la première lettre d'une chaîne en majuscule. Vous pouvez également utiliser la fonction `title()` pour mettre en majuscule la première lettre de chaque mot dans une chaîne de caractères.

## Voir aussi

Maintenant que vous savez comment mettre des majuscules dans vos chaînes de caractères en utilisant Python, voici quelques liens pour approfondir vos connaissances:

- [La documentation officielle de Python sur les chaînes de caractères](https://docs.python.org/fr/3/library/stdtypes.html#string-methods)
- [Un tutoriel sur les chaînes de caractères en Python](https://www.programiz.com/python-programming/string)
- [Une explication détaillée de la méthode `upper()`](https://pynative.com/python-string-upper-method/)
- [Un aperçu de toutes les méthodes pour formater des chaînes en Python](https://realpython.com/python-strings/)