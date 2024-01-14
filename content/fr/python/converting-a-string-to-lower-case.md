---
title:                "Python: Conversion d'une chaîne en minuscules"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut y avoir plusieurs raisons pour lesquelles on voudrait convertir une chaîne de caractères (string) en minuscules en utilisant Python. Cela peut être utile pour le traitement de données textuelles, la comparaison de chaînes de caractères (string), ou pour l'affichage de messages en minuscules.

## Comment faire

Pour convertir une chaîne de caractères (string) en minuscules en utilisant Python, nous pouvons utiliser la fonction `lower()`. Voici un exemple de code et sa sortie :

```Python
my_string = "Python est fantastique!"
print(my_string.lower())
```
```
python est fantastique!
```

Nous pouvons également utiliser la méthode `casefold()` pour une conversion en minuscules plus globale, c'est-à-dire qu'elle prend en compte les caractères spéciaux des différentes langues. Voici un exemple de code et sa sortie :

```Python
my_string = "École De Code"
print(my_string.casefold())
```
```
école de code
```

Enfin, si nous voulons vérifier si une chaîne de caractères (string) est déjà en minuscules, nous pouvons utiliser la méthode `islower()`. Voici un exemple de code et sa sortie :

```Python
my_string = "python est un langage de programmation"
print(my_string.islower())
```
```
True
```

## Plongée en profondeur

Il est important de noter que la conversion en minuscules avec Python est sensible à la langue et à l'encodage utilisé. Par exemple, la chaîne de caractères (string) "École De Code" sera convertie en "école de code" en utilisant la fonction `lower()`, mais en "ecole de code" en utilisant la méthode `casefold()`, car elle est implémentée en utilisant l'encodage ASCII. De plus, la méthode `casefold()` est plus agressive que la fonction `lower()` et peut donc entraîner des résultats différents dans certains cas.

Il est également important de noter que la conversion en minuscules n'est pas une opération réversible. Une fois que nous avons converti une chaîne de caractères (string) en minuscules, nous ne pouvons pas récupérer sa forme originale.

## Voir aussi

- [Documentation officielle de Python sur les méthodes de chaîne de caractères](https://docs.python.org/fr/3.9/library/stdtypes.html#string-methods)
- [Article sur la différence entre `lower()` et `casefold()` en Python](https://realpython.com/python-encodings-guide/#scenario-b-string-manipulation)