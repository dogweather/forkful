---
title:                "Mettre une chaîne en majuscules"
aliases: - /fr/c/capitalizing-a-string.md
date:                  2024-02-03T17:52:58.913386-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre une chaîne en majuscules"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Capitaliser une chaîne en C consiste à convertir le premier caractère de chaque mot dans une chaîne donnée en majuscule s'il s'agit d'une lettre minuscule. Les programmeurs effectuent souvent cette opération pour standardiser l'entrée de l'utilisateur pour les recherches, les opérations de tri, ou les besoins d'affichage, assurant ainsi la cohérence et la lisibilité à travers les données textuelles.

## Comment faire :

Capitaliser une chaîne en C nécessite une compréhension de base de la manipulation de caractères et du parcours de chaîne. Étant donné que C n'a pas de fonction intégrée pour cela, vous vérifierez typiquement chaque caractère, en ajustant sa casse si nécessaire. Voici une implémentation simple :

```c
#include <stdio.h>
#include <ctype.h> // Pour les fonctions islower et toupper

void capitalizeString(char *str) {
    if (str == NULL) return; // Vérification de sécurité
    
    int capNext = 1; // Indicateur pour savoir s'il faut mettre en majuscule la lettre suivante
    for (int i = 0; str[i] != '\0'; i++) {
        if (capNext && islower(str[i])) {
            str[i] = toupper(str[i]); // Mettre le caractère en majuscule
            capNext = 0; // Réinitialiser l'indicateur
        } else if (str[i] == ' ') {
            capNext = 1; // Le caractère suivant doit être mis en majuscule
        }
    }
}

int main() {
    char exampleString[] = "hello world. programming in c!";
    capitalizeString(exampleString);
    printf("Chaîne mise en majuscule : %s\n", exampleString);
    return 0;
}
```

Sortie d'exemple :
```
Chaîne mise en majuscule : Hello World. Programming In C!
```

Ce programme parcourt la chaîne `exampleString`, vérifiant chaque caractère pour savoir s'il doit être mis en majuscule. La fonction `islower` vérifie si un caractère est une lettre minuscule, tandis que `toupper` le convertit en majuscule. L'indicateur `capNext` détermine si la lettre suivante rencontrée doit être convertie, étant défini après chaque espace (' ') trouvé, et initialement pour mettre en majuscule le premier caractère de la chaîne.

## Approfondissement

La technique démontrée est simple mais manque d'efficacité pour des chaînes très longues ou lorsqu'elle est exécutée à plusieurs reprises dans des applications critiques en termes de performance. Dans des contextes historiques et d'implémentation, la manipulation de chaînes en C, y compris la capitalisation, implique souvent une manipulation directe du tampon, reflétant l'approche de bas niveau de C et donnant au programmeur un contrôle total sur les compromis entre mémoire et performance.

Il existe des méthodes alternatives, plus sophistiquées, pour capitaliser les chaînes, en particulier lors de la prise en compte des locales et des caractères unicode, où les règles de capitalisation peuvent différer significativement du simple scénario ASCII. Des bibliothèques telles que ICU (International Components for Unicode) fournissent des solutions robustes pour ces cas mais introduisent des dépendances et des surcharges qui peuvent ne pas être nécessaires pour toutes les applications.

De plus, alors que l'exemple fourni utilise les fonctions de la bibliothèque standard C `islower` et `toupper`, qui font partie de `<ctype.h>`, il est essentiel de comprendre que celles-ci fonctionnent dans la plage ASCII. Pour les applications nécessitant le traitement de caractères au-delà de l'ASCII, comme la gestion des caractères accentués dans les langues européennes, une logique supplémentaire ou des bibliothèques tierces seront nécessaires pour effectuer la capitalisation avec précision.

En conclusion, tandis que la méthode décrite convient pour de nombreuses applications, comprendre ses limites et les alternatives disponibles est crucial pour développer des logiciels robustes et internationalisés en C.
