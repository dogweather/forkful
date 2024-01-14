---
title:    "C: L'écriture des tests"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous programmez en C, vous savez probablement déjà l'importance de tester votre code. Mais pourquoi devriez-vous vous soucier d'écrire des tests ? Eh bien, les tests vous permettent de valider votre code à différentes étapes du développement, ce qui vous aide à vous assurer qu'il fonctionne correctement et qu'il n'y a pas de bugs cachés. De plus, les tests peuvent vous faire économiser du temps à long terme en identifiant rapidement les erreurs lors de la modification du code.

## Comment Faire

L'écriture de tests en C peut sembler intimidante, mais ne vous inquiétez pas, c'est plus facile que vous ne le pensez. Tout d'abord, vous devez avoir une bonne compréhension de la structure du code que vous souhaitez tester. Ensuite, vous pouvez utiliser des assertions, comme ```assert()```, pour vérifier les conditions prédéfinies dans le code. Les assertions sont très utiles pour détecter rapidement les erreurs lors de l'exécution du code.

Par exemple, si vous écrivez une fonction de tri en C, vous pourriez vouloir tester si la fonction fonctionne correctement pour différents types de données, comme les entiers ou les caractères. Dans ce cas, vous pourriez utiliser des assertions pour vérifier si le tableau trié est dans le bon ordre.

```C
#include <stdio.h>
#include <assert.h>

// Fonction de tri basique pour trier des entiers en ordre croissant 
void tri(int tableau[], int taille) {
    int temp;
    for(int i = 0; i < taille-1; i++) {
        for(int j = i+1; j < taille; j++) {
            if(tableau[i] > tableau[j]) {
                temp = tableau[i];
                tableau[i] = tableau[j];
                tableau[j] = temp;
            }
        }
    }
}

// Fonction de test pour vérifier si le tri fonctionne correctement
void test_tri() {
    int tableau[5] = {5, 2, 7, 1, 3};
    int tableau_attendu[5] = {1, 2, 3, 5, 7};
    
    // Utilisation de l'assertion pour vérifier si le tableau est trié correctement
    assert(tri(tableau, 5) == tableau_attendu);
    
    printf("Le test a réussi, le tableau est trié correctement !\n");
}

int main() {
    // Appel de la fonction de test
    test_tri();
    return 0;
}
```
Output:

``` 
Le test a réussi, le tableau est trié correctement ! 
```

## Deep Dive

Lors de l'écriture de tests, il est important de s'assurer que vous testez toutes les parties importantes de votre code. Cela peut sembler fastidieux, mais cela peut vous éviter des problèmes majeurs à l'avenir. Vous pouvez également utiliser des outils de test automatiques, tels que Unity ou CppUTest, pour faciliter l'écriture et l'exécution de tests en C.

De plus, il est important de toujours garder à l'esprit que les tests ne garantissent pas la perfection de votre code. Les tests sont un outil pour vous aider à identifier et à résoudre les erreurs, mais ils ne peuvent pas remplacer une bonne compréhension et une conception solide de votre code.

## Voir Surtout

* [Testing in C - GeeksforGeeks](https://www.geeksforgeeks.org/testing-in-c/)
* [Introduction to Unit Testing in C - CodeProject](https://www.codeproject.com/Articles/857253/Introduction-to-Unit-Testing-in-C)
* [Unity - Unit Testing Framework for C](https://www.throwtheswitch.org/unity)