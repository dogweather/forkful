---
title:                "Extraction de sous-chaînes"
date:                  2024-01-20T17:45:08.918684-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraction de sous-chaînes"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extraire des sous-chaînes, c'est prendre des morceaux spécifiques d'une chaîne de caractères. On fait ça pour analyser, formater ou manipuler des données textes, souvent des entrées d'utilisateurs ou des fichiers.

## How to:
En C, on utilise souvent `strncpy` pour extraire une sous-chaîne. Faites attention à inclure le caractère nul à la fin.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Salut le monde!";
    char buffer[5];

    // Copiez les 4 premiers caractères dans buffer.
    strncpy(buffer, text, 4);
    buffer[4] = '\0'; // N'oubliez pas le caractère de fin de chaîne.

    printf("Sous-chaîne: '%s'\n", buffer);
    return 0;
}
```

Sortie:
```
Sous-chaîne: 'Salu'
```

## Deep Dive
Historiquement, C n'a pas de classe de chaînes de caractères intégrée comme d'autres langues. La bibliothèque standard fournit des fonctions pour travailler avec des chaînes de caractères. Outre `strncpy`, il existe des fonctions comme `strstr` et `strtok` pour la recherche et la découpe de chaînes. Pour éviter les dépassements de tampon, considérez `strncat`, `snprintf`, ou encore, des fonctions de la bibliothèque annexes (comme `strlcpy` dans la lib BSD).

## See Also
- Manuel C en ligne: https://en.cppreference.com/w/c/string/byte
- "The C Programming Language" par Kernighan et Ritchie, souvent appelé K&R, pour une compréhension fondamentale de C.
- GNU C Library: https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html
