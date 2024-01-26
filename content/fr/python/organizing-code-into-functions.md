---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:11:46.748012-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"
programming_language: "Python"
category:             "Python"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/python/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Organiser le code en fonctions consiste à décomposer votre code en morceaux réutilisables ayant des objectifs spécifiques. Nous le faisons pour rendre le code plus propre, plus facile à lire, à déboguer et à mettre à jour.

## Comment faire :
Disons que vous écrivez un script pour calculer le carré et le cube d'un nombre. Sans fonctions, c'est un désordre de répétitions :

```Python
num = 4
square = num * num
cube = num * num * num
print(f"Carré : {square}, Cube : {cube}")

num = 5
square = num * num
cube = num * num * num
print(f"Carré : {square}, Cube : {cube}")
```
Sortie :
```
Carré : 16, Cube : 64
Carré : 25, Cube : 125
```

Avec des fonctions, c'est plus propre :

```Python
def square(n):
    return n * n

def cube(n):
    return n ** 3

num = 4
print(f"Carré : {square(num)}, Cube : {cube(num)}")

num = 5
print(f"Carré : {square(num)}, Cube : {cube(num)}")
```
Sortie :
```
Carré : 16, Cube : 64
Carré : 25, Cube : 125
```

## Examen approfondi
Autrefois, lorsque les programmes étaient simples, vous pouviez vous en sortir en écrivant simplement une liste d'instructions. Mais à mesure que les logiciels devenaient plus complexes, les développeurs se sont rendu compte qu'ils réécrivaient sans cesse le même code. Bonjour les fonctions — des blocs de code réutilisables qui effectuent une seule action.

Les alternatives aux fonctions incluent les classes (regroupement des fonctions avec les données sur lesquelles elles opèrent) et le code en ligne (intelligence exactement là où vous en avez besoin, mais risqué pour des tâches complexes). En ce qui concerne la mise en œuvre, l'astuce ne consiste pas seulement à créer des fonctions, mais à les faire bien exécuter une seule chose — pensez au principe de responsabilité unique. Les fonctions devraient idéalement aussi être sans état, ce qui signifie aucune surprise avec les données entrantes ou sortantes.

## Voir aussi
- Les tutoriels Python officiels sur les fonctions : https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Clean Code' de Robert C. Martin, pour les principes sur comment écrire des fonctions propres.
- 'Refactoring: Improving the Design of Existing Code' de Martin Fowler, qui comprend des exemples d'organisation de code.