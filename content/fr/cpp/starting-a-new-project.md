---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi et Quoi ?
Démarrer un nouveau projet en C++ signifie créer un programme informatique à partir de zéro. Les programmeurs font cela pour résoudre des problèmes spécifiques ou pour créer un logiciel innovant. 

## Comment faire :
Lancement d'un projet C++ simplifié à l'aide des étapes ci-dessous:

```C++
// 1. Installation d'un IDE (par exemple Visual Studio Code)

// 2. Création d'un nouveau fichier de code source .cpp (par exemple main.cpp)
#include<iostream> 
int main() 
{  
    std::cout << "Bonjour, la programmation C++!"; 
    return 0; 
} 
```
Lorsque vous exécutez ce programme, il affichera `Bonjour, la programmation C++!`.

## Plongée profonde
Historiquement, les programmeurs C++ commençaient souvent un nouveau projet en créant simplement un `main.cpp`. Aujourd'hui, pratiquement tous les IDEs modernes permettent de créer un squelette de projet, ce qui rend le processus beaucoup plus fluide.

Une alternative à l'utilisation d'une fonction `main()` tout-en-un serait de créer un projet modulaire à partir de zéro. Bien que cela nécessite plus de travail initial, la modularité facilite la maintenance et le développement futur.

Les détails de l'implémentation peuvent varier en fonction de l'IDE utilisé, des plateformes ciblées (Windows, Linux, MacOS), ainsi que des exigences et des besoins du projet.

## Voir aussi
1. [Documentation C++](https://en.cppreference.com/w/)
2. [Guide de programmation C++](https://www.cplusplus.com/doc/tutorial/)
3. [Visual Studio Code C++ Documentation](https://code.visualstudio.com/docs/languages/cpp)