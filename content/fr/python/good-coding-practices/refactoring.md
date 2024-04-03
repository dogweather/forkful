---
date: 2024-01-26 03:38:04.966916-07:00
description: "Le remaniement est le processus de restructuration du code informatique\
  \ existant\u2014changer le factoring\u2014sans modifier son comportement externe.\
  \ Les\u2026"
lastmod: '2024-03-13T22:44:57.245495-06:00'
model: gpt-4-0125-preview
summary: "Le remaniement est le processus de restructuration du code informatique\
  \ existant\u2014changer le factoring\u2014sans modifier son comportement externe."
title: Remaniement de code
weight: 19
---

## Comment faire :
Supposons que vous ayez un morceau de code qui calcule et imprime la superficie et le périmètre d'un rectangle étant donné sa longueur et sa largeur. Il fait le travail, mais il est répétitif et un peu désordonné.

```python
# Version Originale
length = 4
width = 3

# Calculer la superficie et le périmètre
area = length * width
perimeter = 2 * (length + width)

print("Superficie :", area)
print("Périmètre :", perimeter)
```

Nous pouvons remanier cela en encapsulant la fonctionnalité dans des fonctions, ce qui rend le code plus organisé et réutilisable :

```python
# Version Remaniée

def calculer_superficie(longueur, largeur):
    return longueur * largeur

def calculer_perimetre(longueur, largeur):
    return 2 * (longueur + largeur)

# utilisation
longueur = 4
largeur = 3

print("Superficie:", calculer_superficie(longueur, largeur))
print("Périmètre:", calculer_perimetre(longueur, largeur))
```

Les deux extraits donnent le même résultat :
```
Superficie : 12
Périmètre : 14
```

Mais la version remaniée est plus propre et sépare les préoccupations, ce qui facilite la mise à jour d'un calcul sans affecter l'autre.

## Exploration Approfondie
Le remaniement a ses racines dans les premiers jours du génie logiciel, lorsque les programmeurs ont réalisé que le code pouvait—et devrait—être amélioré même s'il fonctionne déjà "correctement". Le livre fondateur de Martin Fowler "Refactoring: Improving the Design of Existing Code" a articulé de nombreux principes et techniques de base. Il a fameusement dit : "N'importe quel imbécile peut écrire du code qu'un ordinateur peut comprendre. Les bons programmeurs écrivent du code que les humains peuvent comprendre."

Les alternatives au remaniement pourraient inclure la réécriture du code à partir de zéro ou la réalisation de petites modifications sans amélioration systématique. Cependant, le remaniement est généralement plus rentable qu'une réécriture et moins risqué que des modifications ad-hoc. Les détails de mise en œuvre peuvent être spécifiques à chaque paradigme de programmation; cependant, la programmation orientée objet se prête particulièrement bien au remaniement, en particulier avec des techniques telles que l'extraction de méthodes (comme nos fonctions `calculer_superficie` et `calculer_perimetre`), l'inline, le déplacement de fonctionnalités entre objets, et le renommage de méthodes ou de variables pour plus de clarté.

Le remaniement en Python utilise souvent des outils comme `PyCharm`, qui a des capacités de remaniement intégrées, ou `rope`, une bibliothèque Python spécifiquement conçue pour le remaniement. L'utilisation prudente du contrôle de version, tel que `git`, lors du remaniement est fortement conseillée pour suivre les changements de manière incrémentielle.

## Voir Aussi
Pour ceux qui ont soif de plus, plongez dans les ressources suivantes :
- Le livre de Martin Fowler : [Refactoring: Improving the Design of Existing Code](http://www.refactoring.com/)
- Remaniement en Python avec `rope` : [GitHub - rope](https://github.com/python-rope/rope)
- Documentation sur le remaniement de PyCharm : [Jetbrains PyCharm Refactoring Source Code](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru : [Refactoring and Design Patterns](https://refactoring.guru/refactoring)
- Les conférences sur le Code Propre par Uncle Bob (Robert C. Martin) : [Clean Code - Uncle Bob / Leçon 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
