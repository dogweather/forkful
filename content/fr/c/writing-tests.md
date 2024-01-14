---
title:                "C: Ecriture de tests"
simple_title:         "Ecriture de tests"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en herbe ou expérimenté, vous savez probablement que l'écriture de tests est un aspect essentiel de la programmation professionnelle. Mais pourquoi? Eh bien, il y a plusieurs raisons pour lesquelles vous devriez prendre le temps de tester votre code.

Tout d'abord, les tests aident à identifier les erreurs dans votre code avant qu'elles ne deviennent des problèmes dans un environnement de production. Cela signifie que vous pourrez corriger les bugs avant que vos utilisateurs ne les rencontrent, ce qui peut éviter un coût élevé et une mauvaise réputation pour votre entreprise.

Deuxièmement, l'écriture de tests peut également servir de documentation pour votre code et aider d'autres développeurs à comprendre votre travail. De plus, une fois que vous avez écrit des tests pour votre code, vous pouvez les exécuter à tout moment pour vous assurer que toute modification ultérieure ne crée pas de nouveaux bugs.

## Comment faire

Maintenant que vous comprenez pourquoi il est important d'écrire des tests, voyons comment le faire en pratique en utilisant le langage C. Supposons que nous avons une fonction simple qui vérifie si un nombre est pair ou impair et renvoie le résultat.

```
int check_even_odd(int num){
    if(num % 2 == 0){
        return 0; //Pair
    } else {
        return 1; //Impair
    }
}
```

Pour tester cette fonction, nous pouvons créer un fichier de code appelé "test_even_odd.c" où nous incluons notre fonction et exécutons quelques tests pour vérifier si elle fonctionne correctement.

```
#include <stdio.h>
#include "even_odd_func.c"

int main(){
    printf("Vérification du nombre 4 (pair): %d\n", check_even_odd(4));
    printf("Vérification du nombre 7 (impair): %d\n", check_even_odd(7));
    printf("Vérification du nombre 0 (pair): %d\n", check_even_odd(0));
    printf("Vérification du nombre -3 (impair): %d\n", check_even_odd(-3));

    return 0;
}
```

Lorsque nous exécutons ce code, nous obtenons le résultat suivant :

```
Vérification du nombre 4 (pair): 0
Vérification du nombre 7 (impair): 1
Vérification du nombre 0 (pair): 0
Vérification du nombre -3 (impair): 1
```

Nous pouvons voir que notre fonction fonctionne correctement pour les différents cas de test que nous avons choisis. En écrivant des tests comme celui-ci pour chacune de nos fonctions, nous pouvons être confiants que notre code fonctionne correctement.

## Plongée en profondeur

Il existe différents types de tests que vous pouvez écrire pour votre code, tels que des tests unitaires, des tests d'intégration et des tests de validation. Chacun de ces tests a un objectif différent et peut être utilisé à différentes étapes du processus de développement.

Les tests unitaires sont des tests qui s'exécutent sur une seule fonction ou un seul module de code sans dépendre de toute autre partie du code. Les tests d'intégration, quant à eux, vérifient si les différentes parties de votre code fonctionnent bien ensemble. Les tests de validation sont utilisés pour vérifier si le code répond aux exigences du client.

Il existe également des outils et des frameworks de tests spécialement conçus pour le langage C, tels que Unity et CMock, qui peuvent vous aider à écrire et à exécuter des tests plus facilement et de manière plus organisée.

## Voir aussi

- [Introduction aux tests unitaires en C](https://www.embedded.com/introduction-to-unit-testing-for-c-and-cplus/) (en anglais)
- [Tutoriel de test unitaire en C avec Unity](http://movec.de/2011/06/01/test-driven-development-in-c-unity-full-tutorial/) (en anglais)
- [Documentation CMock](https://cortex-m.com/cmock/) (en anglais)