---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:02.668831-07:00
description: "Comment faire : En C, une fonction est d\xE9clar\xE9e avec un type de\
  \ retour, un nom et des param\xE8tres (s'il y en a), suivis d'un bloc de code. Commen\xE7\
  ons par\u2026"
lastmod: '2024-03-13T22:44:58.378279-06:00'
model: gpt-4-0125-preview
summary: "En C, une fonction est d\xE9clar\xE9e avec un type de retour, un nom et\
  \ des param\xE8tres (s'il y en a), suivis d'un bloc de code."
title: Organiser le code en fonctions
weight: 18
---

## Comment faire :
En C, une fonction est déclarée avec un type de retour, un nom et des paramètres (s'il y en a), suivis d'un bloc de code. Commençons par un exemple simple : une fonction qui ajoute deux entiers.

```c
#include <stdio.h>

// Déclaration de la fonction
int add(int a, int b);

int main() {
  int sum = add(5, 3);
  printf("La somme est : %d\n", sum);
  return 0;
}

// Définition de la fonction
int add(int a, int b) {
  return a + b;
}
```

Sortie :
```
La somme est : 8
```

Maintenant, regardons un exemple plus complexe impliquant un type de données personnalisé. Cette fonction calcule la surface d'un rectangle.

```c
#include <stdio.h>

// Définir une structure pour un rectangle
typedef struct {
  int width;
  int height;
} Rectangle;

// Fonction pour calculer la surface d'un rectangle
int calculateArea(Rectangle rect) {
  return rect.width * rect.height;
}

int main() {
  Rectangle myRect = {5, 10};
  int area = calculateArea(myRect);
  printf("La surface du rectangle est : %d\n", area);
  return 0;
}
```

Sortie :
```
La surface du rectangle est : 50
```

## Plongée profonde
Le concept de fonctions en C, hérité des pratiques de programmation antérieures, est fondamental à la programmation structurée. Les fonctions permettent aux développeurs de masquer les détails, de gérer la complexité et d'organiser leur code de manière logique. Depuis son origine, la fonction a été un constructeur central en C, influençant de nombreux autres langages.

Cependant, alors que les paradigmes de programmation ont évolué, des approches alternatives comme la programmation orientée objet (POO) dans des langues telles que C++ et Java, ont étendu le concept de fonctions avec des méthodes associées aux objets. Bien que C ne supporte pas la POO de manière native, il est possible d'imiter les conceptions orientées objet en structurant soigneusement les fonctions et les données.

Dans la programmation moderne, les fonctions restent cruciales, mais avec les avancées dans les optimisations des compilateurs et les fonctionnalités linguistiques, l'accent peut se déplacer vers des fonctions en ligne et des modèles en C++ ou des lambdas dans des langues comme Python et JavaScript. Celles-ci fournissent plus de flexibilité et souvent une syntaxe plus concise pour atteindre une modularité et une réutilisabilité similaires. Toutefois, les principes fondamentaux appris en organisant le code en fonctions en C sont universellement applicables et constituent la base du développement logiciel efficace et performant.
