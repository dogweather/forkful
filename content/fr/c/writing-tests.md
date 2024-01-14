---
title:    "C: Écrire des tests"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi écrire des tests en C?

Écrire des tests dans un programme en C peut sembler fastidieux et chronophage, mais c'est en réalité une étape importante dans le processus de développement. Les tests permettent de vérifier le bon fonctionnement de votre code et de détecter d'éventuels bugs avant même que le programme ne soit déployé. Cela permet non seulement d'économiser du temps et des efforts, mais aussi de garantir un meilleur niveau de qualité pour votre code.

## Comment procéder?

Écrire des tests en C peut sembler intimidant pour les débutants, mais en réalité, il suffit de suivre quelques étapes simples. Tout d'abord, définissez les fonctionnalités que vous souhaitez tester. Ensuite, créez des conditions d'assertion pour chaque fonctionnalité, en vous assurant de couvrir tous les cas possibles. Enfin, exécutez vos tests et vérifiez leur validité en comparant les résultats avec ceux que vous attendez. Voici un exemple de code pour vous aider à comprendre le processus :

```C
#include <stdio.h>
#include <assert.h>

// Fonction à tester
int additionner(int a, int b)
{
    return a + b;
}

// Fonction de test
void test_additionner()
{
    // Vérifier si 2+2 est égal à 4
    assert(additionner(2,2) == 4);
    // Vérifier si 5+3 est égal à 8
    assert(additionner(5,3) == 8);

    // Ajoutez autant de tests que nécessaire pour couvrir tous les cas possibles
}

// Fonction principale
int main()
{
    // Appeler la fonction de test
    test_additionner();
    printf("Tous les tests ont été validés avec succès!");
    return 0;
}
```

Dans cet exemple, nous définissons une fonction `additionner` et vérifions si elle retourne les résultats attendus grâce à la fonction `assert`. Vous pouvez ensuite ajouter autant de fonctions de test que vous le souhaitez pour couvrir toutes les fonctionnalités de votre programme.

## Approfondissement

Il existe également d'autres outils de test disponibles en C, tels que la bibliothèque `ctest` ou les framework de test unitaire comme `check` ou `CppUTest`. Ces outils peuvent faciliter le processus de test en vous permettant de créer des cas de test plus complexes et de générer des rapports détaillés sur les résultats. De plus, il est important de garder à l'esprit que les tests ne doivent pas être considérés comme une tâche secondaire, mais plutôt comme une partie intégrante du cycle de développement.

## Voir aussi

- [Tutoriel sur les tests en C](https://www.tutorialspoint.com/cprogramming/cprogramming_unit_testing.htm)
- [Documentation sur la bibliothèque ctest](https://libcest.sourceforge.io/)
- [Framework de test unitaire check](https://libcheck.github.io/check/index.html)
- [CppUTest framework](https://cpputest.github.io/)