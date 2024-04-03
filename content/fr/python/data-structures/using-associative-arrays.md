---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:39.462667-07:00
description: "Les tableaux associatifs, connus sous le nom de dictionnaires en Python,\
  \ associent des cl\xE9s \xE0 des valeurs, ce qui facilite la r\xE9cup\xE9ration,\
  \ la\u2026"
lastmod: '2024-03-13T22:44:57.227377-06:00'
model: gpt-4-0125-preview
summary: "Les tableaux associatifs, connus sous le nom de dictionnaires en Python,\
  \ associent des cl\xE9s \xE0 des valeurs, ce qui facilite la r\xE9cup\xE9ration,\
  \ la modification ou le suivi des donn\xE9es par un identifiant unique."
title: Utilisation des tableaux associatifs
weight: 15
---

## Quoi et Pourquoi ?

Les tableaux associatifs, connus sous le nom de dictionnaires en Python, associent des clés à des valeurs, ce qui facilite la récupération, la modification ou le suivi des données par un identifiant unique. Les programmeurs les utilisent pour leur efficacité dans l'accès aux éléments et leur flexibilité dans la représentation de structures de données complexes.

## Comment faire :

Créer un dictionnaire en Python est simple. Vous encadrez les paires clé-valeur avec des accolades `{}`, les clés et les valeurs étant séparées par un deux-points :

```Python
# Créer un tableau associatif (dictionnaire)
mon_dict = {"nom": "John", "âge": 30, "ville": "New York"}
print(mon_dict)
```

Sortie :
```
{'nom': 'John', 'âge': 30, 'ville': 'New York'}
```

Accéder à une valeur par sa clé est simple :

```Python
# Accéder à une valeur
print(mon_dict["nom"])
```

Sortie :
```
John
```

Ajouter ou mettre à jour des éléments se fait en assignant une valeur à une clé :

```Python
# Ajouter une nouvelle paire clé-valeur
mon_dict["email"] = "john@exemple.com"
# Mettre à jour une valeur
mon_dict["âge"] = 31
print(mon_dict)
```

Sortie :
```
{'nom': 'John', 'âge': 31, 'ville': 'New York', 'email': 'john@exemple.com'}
```

Pour itérer sur les éléments du dictionnaire :

```Python
# Itérer à travers les paires clé-valeur
for clé, valeur in mon_dict.items():
    print(f"{clé}: {valeur}")
```

Sortie :
```
nom: John
âge: 31
ville: New York
email: john@exemple.com
```

## Exploration plus profonde

Les tableaux associatifs en Python, ou dictionnaires, ont été introduits pour fournir une structure de données permettant un accès et une manipulation efficaces des données. Contrairement aux séquences, qui sont indexées par une plage de nombres, les dictionnaires sont indexés par des clés, qui peuvent être de tout type immuable. Ce choix de conception rend les dictionnaires particulièrement adaptés pour des tables de recherche rapide où les clés correspondent à des valeurs uniques.

Historiquement, les dictionnaires Python ont été implémentés en utilisant une table de hachage, garantissant que le temps moyen de complexité des opérations de recherche, d'insertion et de suppression est de O(1). À partir de Python 3.6, les dictionnaires maintiennent également l'ordre d'insertion des éléments, combinant les avantages des tables de hachage avec la prévisibilité de l'ordre d'insertion, comme cela se voit dans les structures de données ordonnées.

Bien que les dictionnaires soient incroyablement polyvalents, dans certains cas spécialisés, des alternatives comme `collections.defaultdict` ou `collections.OrderedDict` (avant Python 3.7) peuvent être préférables. `defaultdict` est particulièrement utile lorsque vous avez besoin qu'un dictionnaire retourne une valeur par défaut pour les clés inexistantes, simplifiant certains types de logique conditionnelle. Cependant, avec l'amélioration continue et l'évolution de Python, la classe de dictionnaire intégrée reste souvent le choix privilégié pour les tableaux associatifs en raison de sa robustesse et de la commodité qu'elle offre dès le départ.
