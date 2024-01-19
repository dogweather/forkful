---
title:                "Mettre en majuscule une chaîne"
html_title:           "Python: Mettre en majuscule une chaîne"
simple_title:         "Mettre en majuscule une chaîne"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Mettre en majuscule une chaîne de caractères en Python

## Quoi & Pourquoi?
Cette opération consiste à mettre en majuscule le premier caractère d'une chaîne de caractères. Les développeurs l'utilisent pour formater le texte afin de le rendre plus lisible ou de respecter une convention d'écriture.

## Comment faire:
```Python
# Exemple d'une chaîne de caractères en minuscules
sentence = "bonjour tout le monde!"
print(sentence.capitalize())
```
Résultat:
```
Bonjour tout le monde!
```
## Approfondissement
Historiquement, la capitalisation était utilisée dans les langages de programmation pour souligner l'importance d'une variable ou d'une constante. En Python, elle est principalement utilisée pour le formatage de texte.

D'autres méthodes peuvent être utilisées pour mettre en majuscule la première lettre de chaque mot d'une chaîne, comme la fonction title():
```Python
print(sentence.title())
```
Résultat:
```
Bonjour Tout Le Monde!
```
La fonction capitalize() met en majuscule seulement la première lettre de la chaîne, alors que title() met en majuscule la première lettre de chaque mot.

La mise en œuvre de la fonction capitalize() est codée en C dans l'interprèteur Python pour garantir des performances élevées.

## Voir aussi
Pour plus d'informations, consultez la documentation officielle de Python sur les méthodes de chaînes de caractères : [Python String Methods](https://docs.python.org/fr/3/library/stdtypes.html#string-methods)