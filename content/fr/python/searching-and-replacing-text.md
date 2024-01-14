---
title:                "Python: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Python, il est fort probable que vous deviez travailler avec du texte. Mais parfois, nous pouvons nous retrouver face à des tâches fastidieuses telles que la recherche et le remplacement de morceaux de texte dans nos fichiers. Heureusement, il existe une fonctionnalité très utile dans Python qui peut nous faciliter la vie: la recherche et le remplacement de texte.

## Comment faire

Pour rechercher et remplacer du texte en utilisant Python, nous allons utiliser la méthode "replace()" qui peut être appliquée à une chaîne de caractères.

```
# Définir une chaîne de caractères
texte = "Bonjour le monde"

# Rechercher et remplacer "Bonjour" par "Salut"
nouveau_texte = texte.replace("Bonjour", "Salut")

# Afficher le résultat
print(nouveau_texte)
```

La sortie de ce code sera "Salut le monde". Comme vous pouvez le voir, la méthode "replace()" a remplacé toutes les occurrences du mot "Bonjour" par "Salut".

Il est également possible d'utiliser des expressions régulières pour effectuer des recherches et des remplacements plus précis. Dans l'exemple suivant, nous allons utiliser le module "re" pour rechercher et remplacer du texte en utilisant une expression régulière.

```
import re

# Définir une chaîne de caractères
texte = "Bienvenue à Python!"

# Rechercher et remplacer "Python" par "le monde de la programmation"
nouveau_texte = re.sub("Python", "le monde de la programmation", texte)

# Afficher le résultat
print(nouveau_texte)
```

La sortie sera "Bienvenue au monde de la programmation!".

## Plongée en profondeur

Il est important de noter que la méthode "replace()" ne modifie pas la chaîne de caractères d'origine, mais renvoie une nouvelle chaîne de caractères modifiée. Si vous voulez modifier la chaîne de caractères d'origine, vous pouvez assigner la méthode "replace()" à la chaîne de caractères d'origine.

De plus, si vous souhaitez ignorer la casse lors de la recherche et du remplacement de texte, vous pouvez utiliser la méthode "replace()" en combinaison avec les méthodes "lower()" ou "upper()" pour mettre en minuscule ou en majuscule la chaîne de caractères d'origine et effectuer la recherche et le remplacement sur cette nouvelle chaîne.

Enfin, vous pouvez utiliser la méthode "replace()" pour remplacer du texte par une chaîne vide, ce qui aura pour effet de supprimer le texte correspondant.

## Voir aussi

- La documentation officielle de Python sur la méthode "replace()": https://docs.python.org/fr/3/library/stdtypes.html#str.replace
- Le tutoriel sur les expressions régulières en Python: https://docs.python.org/fr/3/howto/regex.html