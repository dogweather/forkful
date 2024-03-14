---
date: 2024-01-26 01:17:00.772174-07:00
description: "Le refactoring est le processus de changement de la structure interne\
  \ d'un programme informatique sans alt\xE9rer son comportement externe. Les programmeurs\u2026"
lastmod: '2024-03-13T22:44:58.172058-06:00'
model: gpt-4-0125-preview
summary: "Le refactoring est le processus de changement de la structure interne d'un\
  \ programme informatique sans alt\xE9rer son comportement externe. Les programmeurs\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Quoi et pourquoi ?

Le refactoring est le processus de changement de la structure interne d'un programme informatique sans altérer son comportement externe. Les programmeurs le font pour nettoyer leur code, le rendant plus facile à comprendre, maintenir et étendre.

## Comment faire :

Imaginez que vous ayez une fonction qui fait un peu trop de choses, comme cette méthode encombrante qui initialise un objet et effectue également des logs :

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Logique d'initialisation
        // ...

        // Logging verbeux
        if (verbose) {
            std::cout << "Widget initialized!" << std::endl;
        }
    }
};

// Utilisation :
Widget w;
w.init(true);
```

Sortie :
```
Widget initialized!
```

Refactoriser cela en méthodes plus propres et plus focalisées pourrait ressembler à ceci :

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Seulement la logique d'initialisation
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget initialized!" << std::endl;
    }
};

// Utilisation :
Widget w;
w.init();
w.logInitialization();
```

Ce changement n'a pas altéré ce que fait le programme mais rend la classe `Widget` plus modulaire et son utilisation plus claire.

## Plongée profonde

Le concept de refactoring tel que nous le connaissons aujourd'hui trouve ses racines dans les communautés de programmation Smalltalk des années 1980 et a été fortement popularisé par le livre de Martin Fowler "Refactoring : Améliorer la conception du code existant" de 1999. Aujourd'hui, le refactoring est une partie intégrante du développement logiciel moderne, intégré dans diverses méthodologies de développement telles que Agile et TDD (Développement piloté par les tests).

Lorsque nous parlons d'alternatives au refactoring, nous nous aventurons sur le terrain de la réécriture ou de la refonte. Le refactoring est stratégique et incrémental, tandis qu'une réécriture peut écarter le code existant en faveur d'une nouvelle solution. La refonte, quant à elle, peut impliquer des changements plus significatifs, y compris la modification de la fonctionnalité, ce qui n'est pas un objectif pour le refactoring pur.

Les détails de mise en œuvre sur le refactoring peuvent devenir assez détaillés. De nombreux 'codes smells' peuvent inciter à un refactoring, tels que des méthodes longues, de grandes classes ou du code dupliqué. Des outils automatisés existent qui peuvent aider au refactoring, comme "Clang-Tidy" pour C++, qui peut identifier les problèmes et même appliquer certaines corrections.

De plus, le refactoring nécessite une solide suite de tests pour garantir que la fonctionnalité reste inchangée. Sans tests, vous volez essentiellement à l'aveugle et risquez des régressions.

## Voir aussi

Pour une compréhension plus approfondie du refactoring et pour voir plus d'exemples, vous pourriez consulter :

- Le texte classique de Martin Fowler "Refactoring : Améliorer la conception du code existant" pour des idées et stratégies de base.
- La documentation de `Clang-Tidy` à https://clang.llvm.org/extra/clang-tidy/ pour un support de refactoring automatisé en C++.
- "Travailler efficacement avec du code ancien" de Michael Feathers, qui fournit des techniques pour le refactoring en toute sécurité dans le contexte de bases de code existantes moins que parfaites.
