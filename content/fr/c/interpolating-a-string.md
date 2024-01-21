---
title:                "Interpolation de chaînes de caractères"
date:                  2024-01-20T17:50:18.213608-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolation de chaînes de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Quoi et Pourquoi ?
L'interpolation de chaînes permet d'insérer des variables dans du texte. Les programmeurs l'utilisent pour créer des messages dynamiques et personnaliser l'affichage sans concaténer manuellement les chaînes.

## How to / Comment faire :
```c
#include <stdio.h>

int main() {
    int userAge = 25;
    char userName[] = "Jean";

    printf("Salut, %s! Tu as %d ans.\n", userName, userAge);

    return 0;
}
```
Sortie:
```
Salut, Jean! Tu as 25 ans.
```

## Deep Dive / Plongée Profonde
Historiquement, C a toujours utilisé `printf` et ses variantes pour l'interpolation de chaînes, bien qu'elles ne soient pas appelées ainsi. Des alternatives incluent les fonctions comme `sprintf` ou `snprintf` qui écrivent dans des chaînes plutôt que dans `stdout`. Pour éviter les débordements de tampon, `snprintf` est préférable. L'interpolation est implémentée en C par une analyse de format spécificateur lequel est indiqué par le symbole `%`.

## See Also / Voir aussi :
- La documentation du C Standard Library sur `printf`: https://en.cppreference.com/w/c/io/fprintf
- Un tutoriel sur les chaînes en C: https://www.tutorialspoint.com/cprogramming/c_strings.htm
- Conseils sur la sécurité avec `snprintf`: https://owasp.org/www-community/attacks/Buffer_overflow_attack