---
title:                "Concaténation de chaînes"
html_title:           "Python: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi?

La concaténation de chaînes est une opération courante en programmation qui consiste à combiner plusieurs chaînes de caractères en une seule. Les programmeurs utilisent souvent la concaténation de chaînes pour créer des messages d'erreur personnalisés, des rapports de données et bien d'autres choses encore.

## Comment faire:

Voici quelques exemples de code en Python pour illustrer comment concaténer des chaînes:

```
# Concaténation de deux chaînes
prenom = "Marie"
nom = "Dupon"
print(prenom + " " + nom) # Output: Marie Dupon

# Concaténation avec un autre type de données
age = 25
print("Je m'appelle " + prenom + " et j'ai " + str(age) + " ans.") # Output: Je m'appelle Marie et j'ai 25 ans.
```

## Plongée en profondeur:

La concaténation de chaînes est un concept qui existe depuis les premiers langages de programmation. À l'époque, les programmeurs devaient utiliser des méthodes spéciales pour concaténer des chaînes, car ce n'était pas une opération directement supportée par le langage lui-même. Heureusement, Python simplifie grandement cette tâche en permettant aux développeurs de concaténer des chaînes simplement en utilisant l'opérateur `+`. Il existe également d'autres méthodes pour concaténer des chaînes, telles que l'utilisation de la méthode `.join()` ou de f-strings.

## Voir aussi:

Pour en savoir plus sur la concaténation de chaînes en Python, vous pouvez consulter la documentation officielle [ici](https://docs.python.org/fr/3/tutorial/introduction.html#types-de-base). Vous pouvez également chercher des tutoriels en ligne pour voir comment d'autres programmeurs utilisent la concaténation de chaînes dans leurs projets.