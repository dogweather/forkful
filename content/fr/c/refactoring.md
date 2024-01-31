---
title:                "Refactoring"
date:                  2024-01-26T01:16:42.977628-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/refactoring.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Le restructuration (ou refactoring) est le processus de réorganisation du code informatique existant sans en changer le comportement externe. Les programmeurs le font pour améliorer la lisibilité, réduire la complexité, ou rendre le code plus maintenable et évolutif, ce qui peut épargner énormément de temps et de maux de tête à l'avenir.

## Comment faire :
Rafraîchissons un peu de code. Imaginez que vous avez une fonction qui calcule la moyenne des entiers dans un tableau. À première vue, c'est un peu un fouillis.

**Avant le Refactoring :**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Somme dans la condition de la boucle for, aïe !
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Moyenne : %f\n", calculateStuff(array, length));

    return 0;
}
```

**Après le Refactoring :**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Moyenne : %f\n", calculateAverage(array, length));
    return 0;
}
```
Même avec cet exemple simple, vous pouvez voir comment séparer la fonction rend le code plus propre et plus maintenable. Chaque fonction a maintenant une responsabilité unique – un principe clé dans la programmation propre.

## Plongée profonde
Le terme "refactoring" a été popularisé dans les années 90, notamment avec la publication du livre de Martin Fowler "Refactoring: Improving the Design of Existing Code." Le refactoring n'implique pas de corriger des bugs ou d'ajouter de nouvelles fonctionnalités, mais plutôt d'améliorer la structure du code.

Il existe de nombreux outils de refactoring et IDEs (Environnements de Développement Intégrés) élégants qui aident à automatiser le processus, comme CLion pour C et C++, mais comprendre ce qui se passe sous le capot reste crucial.

Les alternatives au refactoring peuvent inclure la réécriture du code à partir de zéro (risqué et souvent inutile) ou vivre avec la dette technique (ce qui peut être plus coûteux à long terme). Les détails de mise en œuvre varient selon le projet, mais les refactorisations communes incluent le renommage de variables pour plus de clarté, la division de grandes fonctions en plus petites, et le remplacement de nombres magiques par des constantes nommées.

De plus, des principes comme DRY (Don't Repeat Yourself) et SOLID peuvent guider votre parcours de refactoring, poussant vers une base de code plus facile à tester, à comprendre et sur laquelle collaborer.

## Voir également
Pour plonger plus profondément dans l'océan du refactoring, jetez un œil à :

- La page d'accueil de Martin Fowler : https://martinfowler.com/ avec un trésor d'articles et de ressources sur le refactoring et la conception logicielle.
- Refactoring.com : https://refactoring.com/ offre des exemples et des catalogues de techniques de refactoring.
- Le livre "Refactoring" : Considéré comme une bible du refactoring, sa lecture vous offre une vue complète de la méthodologie.
- "Clean Code: A Handbook of Agile Software Craftsmanship" de Robert C. Martin, qui discute de l'écriture de code facile à comprendre et à maintenir.
