---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:20.313360-07:00
description: "Extraire des sous-cha\xEEnes en C consiste \xE0 cr\xE9er une cha\xEE\
  ne plus petite (sous-cha\xEEne) \xE0 partir d'une cha\xEEne plus grande en fonction\
  \ de crit\xE8res sp\xE9cifi\xE9s,\u2026"
lastmod: '2024-03-13T22:44:58.359144-06:00'
model: gpt-4-0125-preview
summary: "Extraire des sous-cha\xEEnes en C consiste \xE0 cr\xE9er une cha\xEEne plus\
  \ petite (sous-cha\xEEne) \xE0 partir d'une cha\xEEne plus grande en fonction de\
  \ crit\xE8res sp\xE9cifi\xE9s, tels que la position et la longueur."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## Comment faire :
Contrairement à certains langages de haut niveau qui fournissent des méthodes intégrées pour l'extraction de sous-chaînes, C nécessite une approche plus manuelle utilisant ses fonctions de manipulation de chaînes. Voici comment extraire efficacement une sous-chaîne en C :

### Exemple 1 : Utilisation de `strncpy`
```c
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Hello, World!";
    char buffer[20];

    // Extraire "World" de "Hello, World!"
    strncpy(buffer, text + 7, 5);
    buffer[5] = '\0'; // Assurer la terminaison nulle

    printf("Sous-chaîne extraite : %s\n", buffer);
    // Sortie : Sous-chaîne extraite : World
    return 0;
}
```

### Exemple 2 : Création d'une fonction
Pour une utilisation répétée, une fonction dédiée à l'extraction de sous-chaînes peut être plus efficace :

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void extractSubstring(char *source, int from, int n, char *target) {
    strncpy(target, source + from, n);
    target[n] = '\0'; // Assurer la terminaison nulle
}

int main() {
    char text[] = "Programming in C";
    char buffer[50];

    extractSubstring(text, 0, 11, buffer);
    printf("Sous-chaîne extraite : %s\n", buffer);
    // Sortie : Sous-chaîne extraite : Programming
    return 0;
}
```

## Approfondissement
L'extraction de sous-chaînes en C est principalement gérée par la manipulation de pointeurs et une gestion prudente de la mémoire, reflétant l'approche de bas niveau du langage pour manipuler les données. Cette méthode remonte aux premiers jours de la programmation en C, lorsque gérer les ressources de manière efficace était primordial en raison de la puissance de calcul limitée. Bien que l'absence d'une fonction de sous-chaîne intégrée puisse sembler être un oubli, cela illustre la philosophie du C de donner aux programmeurs un contrôle complet sur la gestion de la mémoire, conduisant souvent à un code optimisé mais plus complexe.

Dans le domaine de la programmation moderne, des langages comme Python et JavaScript offrent des méthodes intégrées pour l'extraction de sous-chaînes, telles que `slice()` ou le découpage de chaîne à l'aide d'indices. Ces langages de haut niveau gèrent la gestion de la mémoire en arrière-plan, échangeant une certaine mesure de contrôle contre la facilité d'utilisation et la lisibilité.

Pour les programmeurs en C, comprendre l'arithmétique des pointeurs et l'allocation de mémoire est vital pour des tâches comme l'extraction de sous-chaînes. Bien que cette approche nécessite une compréhension plus approfondie de la façon dont les chaînes sont représentées et manipulées en mémoire, elle offre un contrôle et une efficacité inégalés, des traits caractéristiques de la programmation en C qui l'ont gardé pertinent dans des applications critiques en termes de performances depuis des décennies. Cependant, pour ceux qui travaillent sur des applications de haut niveau où la gestion directe de la mémoire est moins préoccupante, les langages avec des fonctionnalités de sous-chaîne intégrées pourraient offrir une approche plus simple et moins sujette aux erreurs.
