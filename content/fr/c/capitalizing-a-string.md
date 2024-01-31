---
title:                "Mettre une chaîne de caractères en majuscules"
date:                  2024-01-19
simple_title:         "Mettre une chaîne de caractères en majuscules"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Capitaliser une chaîne de caractères, c'est convertir ses lettres en majuscules. On le fait pour normaliser les données, mettre en avant des titres, ou améliorer la lisibilité.

## How to (Comment faire) :
```C
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char* str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "bonjour, le monde!";
    capitalizeString(text);
    printf("Texte capitalisé : %s\n", text);
    return 0;
}
```
Résultat : `Texte capitalisé : BONJOUR, LE MONDE!`

## Deep Dive (Plongée en Profondeur)
Historiquement en C, on manipule beaucoup les chaînes de caractères à bas niveau. Capitaliser une chaîne est une opération classique. La standard library (libc) offre `toupper` pour faciliter le travail. N'oubliez pas les variantes comme `capitalizeWords` pour ne capitaliser que la première lettre de chaque mot, utile pour les titres.

Important : `toupper` n'est pas safe avec des chars signés. Utilisez `(unsigned char)` pour la conversion.

## See Also (Voir aussi)
- Documentation de `toupper`: https://www.cplusplus.com/reference/cctype/toupper/
- GNU C Library: https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html
- Comment manipuler des chaînes en C: https://en.wikibooks.org/wiki/C_Programming/String_manipulation
