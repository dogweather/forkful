---
title:                "La concaténation des chaînes de caractères"
html_title:           "Python: La concaténation des chaînes de caractères"
simple_title:         "La concaténation des chaînes de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir concaténer des chaînes de caractères en Python. Cela peut par exemple être utile pour créer une chaîne de caractères plus longue à partir de plusieurs chaînes plus courtes, ou pour formater des données dans un format spécifique.

## Comment faire

Pour concaténer des chaînes en Python, vous pouvez utiliser l'opérateur `+` ou la méthode `format()`. Voici un exemple de code :

```Python
# Exemple d'utilisation de l'opérateur +
prenom = "Marie"
nom = "Dupont"
nom_complet = prenom + " " + nom
print(nom_complet) # Résultat : "Marie Dupont"

# Exemple d'utilisation de la méthode format()
age = 25
message = "J'ai {} ans.".format(age)
print(message) # Résultat : "J'ai 25 ans."
```

## Plongée en profondeur

Il est important de noter que les chaînes de caractères en Python sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées directement. Ainsi, chaque fois que vous concaténez des chaînes, une nouvelle chaîne est créée plutôt que de modifier la chaîne existante.

De plus, l'opérateur `+` est généralement plus rapide à utiliser que la méthode `format()`, car cette dernière doit d'abord analyser la chaîne et remplacer les placeholders `{}` par les valeurs données.

## Voir également

- [Documentation officielle de Python sur les chaînes de caractères](https://docs.python.org/fr/3/library/stdtypes.html#string-methods)
- [Un tutoriel sur la manipulation de chaînes en Python](https://realpython.com/python-strings/)
- [Une vidéo explicative sur la concaténation de chaînes en Python](https://www.youtube.com/watch?v=U11AyzctcF8)