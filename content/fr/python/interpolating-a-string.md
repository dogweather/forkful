---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'interpolation de chaînes en Python est une façon d'insérer des variables dans une chaîne. Elle est utilisée pour générer des chaînes dynamiques et faciliter la lecture du code.

## Comment faire:

Python fournit plusieurs façons d'interpoler les chaînes. On peut par exemple utiliser le formatage de chaînes avec l'opérateur `%` ou bien avec la méthode `.format()`. Python 3.6 a introduit les f-strings, une manière plus simple et plus efficace.

Voici quelques exemples:

```Python
# Avec l'opérateur %
name = "Pierre"
print("Bonjour %s" % name)
# Sortie: Bonjour Pierre

# Avec la méthode .format()
print("Bonjour {}".format(name))
# Sortie: Bonjour Pierre

# Avec les f-strings
print(f"Bonjour {name}")
# Sortie: Bonjour Pierre
```

## Plongée plus profonde:

Historiquement, l'opérateur `%` était la seule façon de faire de l'interpolation de chaînes en Python. Cependant, cette méthode peut être source de confusion et d'erreurs. Python 3 a introduit la méthode `.format()`, qui est plus lisible et flexible. 

Avec Python 3.6, les f-strings sont arrivées, offrant une syntaxe plus simple et une meilleure performance. Elles permettent également l'évaluation d'expressions à l'intérieur des accolades. 

```Python
age = 20
# Avec les f-strings, on peut faire ceci:
print(f"Vous avez {age}. Dans 10 ans, vous aurez {age+10}")
# Sortie: Vous avez 20. Dans 10 ans, vous aurez 30
```

Il existe des alternatives à l'interpolation de chaînes en Python, comme la concaténation de chaînes avec l'opérateur `+` ou la méthode `join()`. Néanmoins, ces méthodes sont souvent moins efficaces et moins lisibles.

## Voir aussi:

- Documentation officielle de Python sur le formatage des chaînes: https://docs.python.org/fr/3.9/tutorial/inputoutput.html#fancier-output-formatting
- Tutoriels Python sur les f-strings: https://realpython.com/python-f-strings/
- Tutoriels Python sur la méthode .format(): https://pyformat.info/