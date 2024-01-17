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

##Quoi & Pourquoi?
La suppression de caractères correspondant à un motif est un élément fondamental de la manipulation de chaînes de caractères en Python. Cela permet aux programmeurs d'effectuer des modifications spécifiques sur des caractères dans une chaîne, en fonction d'un motif défini par l'utilisateur.

##Comment faire:
Python offre plusieurs méthodes pour supprimer des caractères correspondant à un motif dans une chaîne de caractères. Voici un exemple de code avec la méthode `.strip()`:

```Python
# Définir une chaîne de caractères
mot = 'bonjour à tous!'

# Utiliser la méthode .strip() pour supprimer les caractères 'o' et '!'
nouveau_mot = mot.strip('o!')

# Afficher le résultat
print(nouveau_mot)

# Output: bnjour à tus
```

##Plongée en profondeur:
La suppression de caractères correspondant à un motif est une fonctionnalité étendue dans la manipulation de chaînes de caractères, et peut être réalisée de différentes façons. Avant l'introduction de la méthode `.strip()` en Python 3, il était courant d'utiliser des boucles et des expressions régulières pour effectuer cette tâche.

D'autres alternatives incluent les méthodes `.replace()`, `.translate()` et l'utilisation de la bibliothèque `re` pour les expressions régulières plus complexes.

##Voir aussi:
- [Documentation officielle de Python sur les méthodes de chaînes de caractères](https://docs.python.org/fr/3/library/stdtypes.html#text-sequence-type-str)
- [Guide sur les expressions régulières en Python](https://realpython.com/regex-python/)
- [Tutoriel sur les méthodes de chaînes de caractères en Python](https://www.programiz.com/python-programming/string)