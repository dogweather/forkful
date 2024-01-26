---
title:                "Réusinage"
date:                  2024-01-26T01:39:01.842026-07:00
model:                 gpt-4-0125-preview
simple_title:         "Réusinage"
programming_language: "Java"
category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/refactoring.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La refactorisation est le processus de restructuration du code informatique existant—changer la facturation—sans modifier son comportement externe. Les programmeurs le font pour améliorer les attributs non fonctionnels du logiciel, en améliorant la lisibilité, réduisant la complexité, et rendant le code plus facile à maintenir pour les futures entreprises.

## Comment :
Prenons une simple classe Java qui crie à la refactorisation en raison de sa mauvaise organisation et de son manque de clarté.

```java
public class Calculator {
    public int calc(int op1, int op2, String operation) {
        if (operation.equals("add")) {
            return op1 + op2;
        } else if (operation.equals("subtract")) {
            return op1 - op2;
        } // D'autres opérations...
    }
}
```

Après la refactorisation, nous avons :

```java
public class Calculator {
    public int add(int operand1, int operand2) {
        return operand1 + operand2;
    }

    public int subtract(int operand1, int operand2) {
        return operand1 - operand2;
    }

    // D'autres opérations...
}
```

En refactorisant, nous avons amélioré les noms des méthodes et les paramètres pour la lisibilité et éliminé le besoin d'une branche conditionnelle au sein d'une seule méthode. Chaque opération indique désormais clairement son but.

## Plongée Profonde :
La refactorisation a ses racines dans la communauté Smalltalk, avec son accent sur la lisibilité du code et la conception orientée objet, mais elle a vraiment décollé dans le monde Java à la fin des années '90 et au début des années '00, particulièrement après la publication du livre séminal de Martin Fowler, "Refactoring: Improving the Design of Existing Code."

Il existe des alternatives à la refactorisation, comme réécrire le code à partir de zéro. Cependant, la refactorisation est souvent préférée car elle implique des changements incrémentiels qui ne perturbent pas la fonctionnalité de l'application.

Les détails de mise en œuvre lors de la refactorisation en Java (ou n'importe quel langage de programmation) tournent autour de la compréhension des odeurs de code—indicateurs de problèmes plus profonds dans le code. Certaines odeurs incluent les méthodes longues, les grandes classes, le code dupliqué et l'utilisation excessive de primitives. En appliquant des modèles de refactorisation tels qu'Extraire Méthode, Déplacer Méthode, ou Remplacer Temp par Requête, les développeurs peuvent aborder systématiquement ces odeurs tout en assurant que le code reste fonctionnel à tout moment.

Les outils automatisés, comme le support de refactorisation d'IntelliJ IDEA, ou les plugins pour Eclipse, peuvent faciliter le processus en automatisant des refactorisations telles que renommer des variables, méthodes, et classes, extraire des méthodes ou variables, et déplacer des méthodes ou classes vers différents paquets ou espaces de noms.

## Voir Aussi :
- "Refactoring: Improving the Design of Existing Code" de Martin Fowler : https://martinfowler.com/books/refactoring.html
- Techniques de refactorisation sur Refactoring.Guru : https://refactoring.guru/refactoring/techniques
- Refactorisation automatisée dans Eclipse : https://www.eclipse.org/eclipse/news/4.18/jdt.php
- Fonctionnalités de refactorisation d'IntelliJ IDEA : https://www.jetbrains.com/idea/features/refactoring.html

Chacune de ces ressources fournit soit une base pour comprendre les principes de la refactorisation soit des outils qui peuvent être exploités pour mettre ces principes en pratique.