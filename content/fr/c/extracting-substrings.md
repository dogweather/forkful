---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?
L'extraction de sous-chaînes est une technique consistant à récupérer une certaine portion d'une chaîne de caractères. Les développeurs l'utilisent pour analyser et manipuler des données complexes.

## Comment faire:
Voici un exemple de comment vous pouvez extraire des sous-chaînes en C.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Salut monde!";
    char str2[10];

    strncpy(str2, str1, 5);
    str2[5] = '\0';   // Ajouter le caractère nul à la fin de str2

    printf("%s\n", str2); //Output: Salut

    return 0;
}
```
Dans cet exemple, nous avons copié les 5 premiers caractères de "Salut monde!" dans `str2`. La sortie affichera "Salut".

## Approfondissement:
L'extraction de sous-chaînes est une méthode souvent utilisée en informatique depuis la naissance du langage C dans les années 70. Les alternatives à la fonction `strncpy` incluent la function `memcpy`, que vous pouvez utiliser si vous manipulez des données binaires non formatées, et la fonction `sprintf` qui, avec le bon format, peut également extraire des sous-chaînes.

Lors de l'utilisation de `strncpy`, n'oubliez pas d'ajouter le caractère nul à la fin de la sous-chaîne. Si vous omettez cette étape, vous obtiendrez une chaîne mal formée qui peut causer des bugs difficiles à traquer.

## Voir aussi:
- [La documentation C de strncpy](http://www.cplusplus.com/reference/cstring/strncpy/)
- [Un guide pour comprendre les strings en C](https://www.learncpp.com/cpp-tutorial/an-introduction-to-stdstring/)
- [La fonction memcpy](https://en.cppreference.com/w/c/string/byte/memcpy)
- [La fonction sprintf](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)