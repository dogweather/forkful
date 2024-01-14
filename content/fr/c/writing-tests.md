---
title:                "C: Écriture de tests"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur C, vous savez sûrement combien il est important de tester votre code. Les tests vous permettent de vérifier si votre code fonctionne correctement et de prévenir les erreurs avant qu'elles ne se produisent. Dans cet article, nous allons plonger dans le monde des tests en C et discuter de leur importance.

## Comment

Pour tester en C, il existe différentes bibliothèques telles que CUnit et Unity qui peuvent vous aider. Prenons par exemple l'utilisation de CUnit pour tester une fonction simple qui calcule la moyenne de deux nombres :

```C
#include <stdio.h>
#include "CUnit/Basic.h"

float moyenne(float a, float b){
  return (a + b) / 2;
}

void test_moyenne(){
  CU_ASSERT_EQUAL(moyenne(4, 6), 5);
}

int main(){
  CU_initialize_registry();
  CU_pSuite suite = CU_add_suite("suite", NULL, NULL);
  CU_add_test(suite, "test_moyenne", test_moyenne);
  CU_basic_run_suite(suite);
  CU_cleanup_registry();
  return 0;
}
```

La partie ```C#include <stdio.h>``` vous permet d'utiliser la fonction ```CUnit/Basic.h```, qui contient toutes les fonctions nécessaires pour effectuer vos tests. Ensuite, nous définissons notre fonction moyenne et écrivons notre test dans ```Cvoid test_moyenne()```. Nous utilisons la fonction ```CCU_ASSERT_EQUAL``` pour comparer le résultat de notre fonction avec la valeur attendue de 5. Enfin, dans la fonction principale, nous initialisons notre registre de tests, ajoutons notre suite de tests, exécutons les tests et nettoyons le registre.

## Deep Dive

Pour écrire des tests efficaces en C, il est important de comprendre certains concepts tels que le TDD (Test Driven Development) et les différentes méthodes de test telles que le test unitaire, le test d'intégration et le test fonctionnel. Le TDD consiste à écrire les tests avant d'écrire le code, ce qui peut vous aider à mieux structurer votre code et à l'améliorer. Les méthodes de test quant à elles sont différentes façons de tester votre code à différents niveaux, afin de garantir un fonctionnement correct.

Lors de l'écriture de tests en C, il est également important d'utiliser des outils tels que Valgrind pour détecter les fuites de mémoire et les erreurs de segmentation dans votre code. Il est également recommandé de tester les bords et les cas extrêmes, afin de s'assurer que votre code fonctionne dans toutes les situations.

## Voir aussi

- [CUnit](https://sourceforge.net/projects/cunit/)
- [Unity](https://github.com/ThrowTheSwitch/Unity)
- [Valgrind](http://valgrind.org/)