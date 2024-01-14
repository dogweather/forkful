---
title:    "Python: Convertir une chaîne en minuscules"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion de chaînes de caractères en minuscules (lower case) est un élément essentiel de la programmation en Python. Cela permet de normaliser les données saisies par les utilisateurs, de faciliter la recherche et le tri de données ou encore de garantir un comportement cohérent du code.

## Comment faire

Il existe plusieurs façons de convertir une chaîne de caractères en minuscules en Python, mais voici une méthode simple et efficace :

```Python
# Déclaration de la chaîne de caractères initiale
str = "BONJOUR TOUT LE MONDE"
# Conversion en minuscules
lower_str = str.lower()
# Affichage de la chaîne en minuscules
print(lower_str)
```

Résultat :

```
bonjour tout le monde
```

Il est également possible d'utiliser la méthode `str.casefold()` pour une conversion en minuscules plus complète et adaptée à différentes langues, notamment pour la gestion des caractères accentués.

## Plongée en profondeur

La méthode `str.lower()` utilise la table de correspondance ASCII pour effectuer la conversion en minuscules des caractères alphabétiques. Ainsi, les lettres accentuées en français seront remplacées par leur équivalent non accentué.

Il est important de noter que la méthode `str.lower()` ne modifie pas la chaîne de caractères initiale, mais en renvoie une nouvelle version en minuscules. Si l'on souhaite modifier directement la chaîne d'origine, il faut utiliser la méthode `str.lower()` combinée à la réaffectation de la variable :

```Python
str = "BONJOUR TOUT LE MONDE"
str = str.lower()
print(str) # Bonjour tout le monde
```

## Voir aussi

- Documentation officielle de Python sur la méthode `str.lower()` : https://docs.python.org/fr/3/library/stdtypes.html#str.lower
- Article sur la normalisation des données en Python : https://towardsdatascience.com/data-preprocessing-in-python-a-complete-guide-2a574bbfcbfb
- Tutoriel sur les méthodes de manipulation des chaînes de caractères en Python : https://realpython.com/python-strings/