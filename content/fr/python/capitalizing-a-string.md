---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
La mise en majuscule d'une chaîne signifie convertir la première lettre d'un texte en majuscule, souvent pour des raisons de formatage ou de normes grammaticales. Les programmeurs l'utilisent pour améliorer la lisibilité des noms propres, des titres ou suivre des conventions de codage.

## Comment faire :
Voici comment transformer une chaîne en majuscules en Python :

```python
chaine = "bonjour le monde"
chaine_maj = chaine.capitalize()
print(chaine_maj)  # Bonjour le monde

# Autre méthode pour tous les mots
chaine_titre = chaine.title()
print(chaine_titre)  # Bonjour Le Monde
```

Résultats :
```
Bonjour le monde
Bonjour Le Monde
```

## Exploration plus profonde
Historiquement, la mise en majuscule est une règle de typographie qui remonte à des siècles. En Python, `.capitalize()` et `.title()` sont des méthodes introduites pour manipuler les chaînes de caractères. `.capitalize()` transforme uniquement la première lettre de la chaîne en majuscule, tandis que `.title()` applique cette transformation à la première lettre de chaque mot.

D'un point de vue technique, ces méthodes itèrent à travers la chaîne, vérifiant chaque caractère et le modifiant si nécessaire. Cela respecte les règles Unicode pour la correspondance des caractères.

Des alternatives existent, comme `.upper()` qui met tout en majuscules ou `.lower()` pour tout mettre en minuscules. Certaines bibliothèques tierces offrent plus de fonctions avancées s'adaptant à des cas spécifiques.

La mise en majuscule ne se limite pas à la langue anglaise ; Python gère correctement les caractères spéciaux et les accents, importants en français.

## Voir aussi
- Documentation Python sur les chaînes de caractères: https://docs.python.org/fr/latest/library/stdtypes.html#string-methods
- Guide de style PEP 8 pour la nomenclature de variables en Python: https://peps.python.org/pep-0008/#naming-conventions
- Un tutoriel sur l'utilisation des strings en Python: https://realpython.com/python-strings/