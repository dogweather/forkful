---
title:                "Mise en majuscule d'une chaîne de caractères"
html_title:           "Python: Mise en majuscule d'une chaîne de caractères"
simple_title:         "Mise en majuscule d'une chaîne de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Quel est l'intérêt de capitaliser une chaîne de caractères et pourquoi les programmeurs le font

Capitaliser une chaîne de caractères signifie mettre en majuscule la première lettre de chaque mot. Les programmeurs le font pour des raisons de lisibilité et pour suivre les conventions de codage. Cela peut également être utile lors de la comparaison de chaînes de caractères, où une différence de casse peut entraîner un résultat incorrect.

# Comment faire :

Voici un exemple de code en Python montrant comment capitaliser une chaîne de caractères :

```
string = "bonjour, je suis un programmeur"
string.capitalize() # renvoie "Bonjour, je suis un programmeur"
```

Et voici un autre exemple avec une chaîne de caractères comprenant un mot déjà en majuscule :

```
string = "Python est un Langage de Programmation"
string.capitalize() # renvoie "Python est un langage de programmation"
```

# Plongée en profondeur :

Historiquement, la capitalisation de chaînes de caractères était principalement utilisée pour des raisons esthétiques et de convention. Cependant, avec l'avènement de la programmation orientée objet, la méthode `capitalize()` est devenue plus utile dans le cadre de la comparaison de chaînes de caractères. Il existe également d'autres méthodes pour capitaliser une chaîne de caractères, telles que `upper()` qui met en majuscule toute la chaîne, et `title()` qui met en majuscule la première lettre de chaque mot.

# À voir aussi :

Voici quelques ressources utiles pour en savoir plus sur la capitalisation des chaînes de caractères en Python :

- [Documentation officielle de la méthode `capitalize()`](https://docs.python.org/fr/3/library/stdtypes.html#str.capitalize)
- [Article sur les méthodes pour manipuler les chaînes de caractères en Python](https://realpython.com/python-strings/)

N'hésitez pas à explorer et à découvrir d'autres méthodes pour manipuler les chaînes de caractères en Python !