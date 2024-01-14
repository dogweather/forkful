---
title:    "Python: Majuscule d'une chaîne de caractères."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Pourquoi

Il est important de savoir comment capitaliser une chaîne en Python car cela peut être utile dans plusieurs situations, notamment lorsque vous travaillez avec des données textuelles et que vous souhaitez mettre en évidence certaines parties du texte. Cela peut également rendre vos résultats plus lisibles et faciles à comprendre.

## Comment faire

Pour capitaliser une chaîne en Python, vous pouvez utiliser la méthode built-in `capitalize()` comme ceci :

```Python
my_string = "python est un langage très puissant"
print(my_string.capitalize())
```

La sortie de ce code sera :

```
Python est un langage très puissant
```

Vous pouvez également utiliser la méthode `title()` pour capitaliser chaque mot dans une chaîne :

```Python
my_string = "python est un langage très puissant"
print(my_string.title())
```

La sortie de ce code sera :

```
Python Est Un Langage Très Puissant
```

Si vous voulez capitaliser seulement la première lettre de chaque mot, vous pouvez utiliser la méthode `split()` pour diviser la chaîne en mots individuels, puis capitaliser la première lettre de chaque mot avec `capitalize()`, et ensuite joindre les mots ensemble avec la méthode `join()` :

```Python
my_string = "python est un langage très puissant"
my_list = my_string.split()
my_new_string = " ".join([word.capitalize() for word in my_list])
print(my_new_string)
```

La sortie de ce code sera :

```
Python Est Un Langage Très Puissant
```

## Plongée en profondeur

Bien qu'il soit assez simple de capitaliser une chaîne en utilisant les méthodes `capitalize()` ou `title()`, il est important de noter que ces méthodes ne fonctionnent que pour les chaînes contenant des lettres en minuscules. Si votre chaîne contient des lettres en majuscules, ces méthodes ne modifieront pas ces lettres.

De plus, si votre chaîne contient des caractères spéciaux ou des chiffres, ces méthodes ne les modifieront pas non plus. Cela peut être un problème si vous voulez ne capitaliser que les lettres dans une chaîne et ignorer tous les autres caractères.

Vous pouvez également utiliser la méthode `swapcase()` pour inverser les lettres majuscules en minuscules et vice versa dans une chaîne. Cela peut être utile si vous souhaitez transformer entièrement une chaîne en majuscules ou en minuscules.

## Voir aussi

- [Documentation officielle Python pour la méthode capitalize()](https://docs.python.org/fr/3/library/stdtypes.html#str.capitalize)
- [Documentation officielle Python pour la méthode title()](https://docs.python.org/fr/3/library/stdtypes.html#str.title)
- [Documentation officielle Python pour la méthode swapcase()](https://docs.python.org/fr/3/library/stdtypes.html#str.swapcase)