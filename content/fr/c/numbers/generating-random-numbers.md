---
title:                "Génération de nombres aléatoires"
aliases:
- fr/c/generating-random-numbers.md
date:                  2024-02-03T17:57:04.454607-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/generating-random-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Générer des nombres aléatoires en C implique de créer des valeurs imprévisibles et suivant une distribution spécifique, telle que uniforme ou normale. Cette capacité est cruciale pour des applications allant des simulations et jeux aux opérations cryptographiques, où l’imprévisibilité ou la simulation du hasard du monde réel est essentielle.

## Comment faire :

En C, des nombres aléatoires peuvent être générés en utilisant la fonction `rand()`, qui fait partie de la bibliothèque standard du C `<stdlib.h>`. Par défaut, `rand()` produit des nombres pseudo-aléatoires dans l’intervalle de 0 à `RAND_MAX` (une constante définie dans `<stdlib.h>`). Pour plus de contrôle sur l’intervalle, les programmeurs peuvent manipuler la sortie de `rand()`.

Voici un exemple simple de génération d'un nombre aléatoire entre 0 et 99 :

```c
#include <stdio.h>
#include <stdlib.h> // Pour rand() et srand()
#include <time.h>   // Pour time()

int main() {
    // Initialiser le générateur de nombres aléatoires
    srand((unsigned) time(NULL));

    // Générer un nombre aléatoire entre 0 et 99
    int randomNumber = rand() % 100;

    printf("Nombre Aléatoire : %d\n", randomNumber);

    return 0;
}
```

Le résultat d'exécution pourrait varier à chaque fois que vous exécutez ce programme :

```
Nombre Aléatoire : 42
```
Pour générer des nombres aléatoires dans un intervalle différent, vous pouvez ajuster l'opérateur modulo (`%`) en conséquence. Par exemple, `rand() % 10` génère des nombres de 0 à 9.

Il est important de noter que d’initialiser le générateur de nombres pseudo-aléatoires (`appel à srand()`) avec l'heure actuelle (`time(NULL)`) garantit des séquences différentes de nombres aléatoires à travers les exécutions du programme. Sans initialisation (`srand()`), `rand()` produirait la même séquence de nombres à chaque fois que le programme est exécuté.

## Plongée profonde

La fonction `rand()` et son homologue d'initialisation `srand()` font partie de la bibliothèque standard du C depuis des décennies. Elles sont basées sur des algorithmes qui génèrent des séquences de nombres qui semblent seulement être aléatoires—d’où le terme "pseudo-aléatoire". L'algorithme sous-jacent dans `rand()` est typiquement un générateur linéaire congruentiel (GLC).

Bien que `rand()` et `srand()` soient suffisants pour de nombreuses applications, ils présentent des limitations connues, en particulier concernant la qualité de l’aléatoire et la potentielle prévisibilité. Pour des applications nécessitant un haut niveau de randomicité, telles que les opérations cryptographiques, des alternatives comme `/dev/random` ou `/dev/urandom` (sur des systèmes de type Unix), ou des API fournies par des bibliothèques cryptographiques, devraient être envisagées.

Avec l'introduction de C11, la norme ISO C a inclus un nouvel en-tête, `<stdatomic.h>`, offrant un contrôle plus raffiné pour les opérations concurrentes, mais ne concernant pas directement l’aléatoire. Pour une vraie randomicité en C, les développeurs se tournent souvent vers des bibliothèques spécifiques à la plateforme ou externes qui offrent de meilleurs algorithmes ou utilisent des sources d'entropie matérielles.

Rappelez-vous, alors que `rand()` sert de moyen simple et accessible pour générer des nombres pseudo-aléatoires, ses utilisations dans les applications modernes sont limitées par la qualité et la prévisibilité de sa sortie. Quand des solutions plus robustes sont requises, spécialement pour des applications soucieuses de sécurité, explorer au-delà de la bibliothèque standard est fortement recommandé.
