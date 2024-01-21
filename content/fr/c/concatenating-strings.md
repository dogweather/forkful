---
title:                "Concaténation de chaînes de caractères"
date:                  2024-01-20T17:34:39.846452-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Concaténer des chaînes, c'est les assembler bout à bout, pour former une seule chaîne. Les programmeurs le font pour afficher des messages complexes, stocker des données ou pour construire dynamiquement des requêtes.

## Comment faire :
```C
#include <stdio.h>
#include <string.h>

int main() {
    char debut[] = "C'est ";
    char fin[] = "genial!";
    char resultat[50];

    strcpy(resultat, debut); // On initialise resultat avec le début
    strcat(resultat, fin); // On ajoute la fin à la suite

    printf("La chaîne concaténée est : %s\n", resultat);
    return 0;
}
```
Sortie :
```
La chaîne concaténée est : C'est genial!
```

## Plongée Profonde
Historiquement, le C n'avait pas de fonctions dédiées à la concaténation de chaînes. Les développeurs devaient manipuler les chaînes manuellement avec des boucles et des pointeurs. Aujourd'hui, la bibliothèque standard propose `strcpy` et `strcat`, mais attention aux débordements ('buffer overflows') ! Alternativement, on utilise `strncat` pour plus de sécurité. Les chaînes en C sont des tableaux de caractères terminés par '\0' (le caractère nul), ce qui signifie que toute fonction de manipulation doit respecter cette convention.

## Voir Aussi
- [Documentation of strcpy](https://www.cplusplus.com/reference/cstring/strcpy/)
- [Documentation of strcat](https://www.cplusplus.com/reference/cstring/strcat/)
- [Secure coding in C](https://www.cert.org/secure-coding/)