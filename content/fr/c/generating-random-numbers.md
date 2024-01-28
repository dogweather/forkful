---
title:                "Génération de nombres aléatoires"
date:                  2024-01-27T20:32:56.008037-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Générer des nombres aléatoires en C consiste à créer des séquences de nombres sans aucun motif discernable, imitant le concept de l'aléatoire. Les programmeurs utilisent des nombres aléatoires à des fins variées, notamment la simulation de données, les applications cryptographiques et le développement de jeux, ce qui en fait un aspect vital de la programmation.

## Comment faire :

Pour générer des nombres aléatoires en C, vous utilisez typiquement la fonction `rand()` trouvée dans `stdlib.h`. Cependant, il est crucial d'initialiser le générateur de nombres aléatoires pour garantir la variabilité des nombres générés à travers différentes exécutions du programme. La fonction `srand()`, initialisée avec une valeur, souvent l'heure actuelle, facilite cela.

Voici un exemple simple de génération d'un nombre aléatoire entre 0 et 99 :

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Initialiser le générateur de nombres aléatoires
    srand((unsigned) time(NULL));

    // Générer un nombre aléatoire entre 0 et 99
    int randomNumber = rand() % 100;

    // Afficher le nombre aléatoire
    printf("Nombre Aléatoire : %d\n", randomNumber);

    return 0;
}
```

Exemple de sortie :

```
Nombre Aléatoire : 42
```

Il est important de noter que chaque exécution de ce programme produira un nouveau nombre aléatoire, grâce à l'initialisation avec l'heure actuelle.

## Approfondissement

La manière traditionnelle de générer des nombres aléatoires en C, en utilisant `rand()` et `srand()`, n'est pas véritablement aléatoire. Elle est pseudo-aléatoire. Cela convient pour de nombreuses applications, mais cela ne suffit pas dans des situations nécessitant un haut degré d'aléatoire, comme dans les usages cryptographiques sérieux. La séquence générée par `rand()` est entièrement déterminée par la graine fournie à `srand()`. Ainsi, si la graine est connue, la séquence peut être prédite, réduisant l'aléatoire.

Historiquement, la fonction `rand()` a été critiquée pour la faible qualité de son aléatoire et sa gamme limitée. Les alternatives modernes incluent l'utilisation d'API spécifiques aux appareils ou de bibliothèques externes qui approximent mieux le véritable aléatoire, ou, dans les systèmes de type UNIX, la lecture à partir de `/dev/random` ou `/dev/urandom` à des fins cryptographiques.

Par exemple, en utilisant `/dev/urandom` en C :

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // Ouvrir /dev/urandom pour la lecture
    fp = fopen("/dev/urandom", "r");

    // Lire un nombre aléatoire
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // Afficher le nombre aléatoire
    printf("Nombre Aléatoire : %u\n", randomNumber);

    // Fermer le fichier
    fclose(fp);

    return 0;
}
```

Cette méthode lit directement à partir du pool d'entropie du système, offrant une qualité d'aléatoire supérieure adaptée à des applications plus sensibles. Cependant, cette approche peut avoir des problèmes de portabilité sur différentes plateformes, la rendant moins universelle que l'utilisation de `rand()`.

Indépendamment de la méthode, comprendre la nature de l'aléatoire et son implémentation en C est crucial pour développer des applications efficaces, sécurisées et captivantes.
