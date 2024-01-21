---
title:                "Recherche et remplacement de texte"
date:                  2024-01-20T17:57:10.375705-07:00
model:                 gpt-4-1106-preview
simple_title:         "Recherche et remplacement de texte"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? | Quoi & Pourquoi ?
La recherche et le remplacement de texte consistent à localiser des chaînes spécifiques dans un texte et à les remplacer par d'autres. Les programmeurs l'utilisent pour corriger des erreurs, mettre à jour des données ou automatiser des modifications dans des masses de code ou des documents.

## How to: | Comment faire :
```C
#include <stdio.h>
#include <string.h>

void searchAndReplace(char *text, const char *search, const char *replace) {
    char buffer[1024];
    char *p;

    if(!(p = strstr(text, search))) {
        // No match found
        printf("%s", text);
        return;
    }

    *p = '\0'; // terminate string at the start of the match
    snprintf(buffer, sizeof(buffer), "%s%s%s", text, replace, p + strlen(search));
    printf("%s", buffer);
}

int main() {
    char text[] = "Les chaussettes de l'archiduchesse, sont-elles sèches? Archi-sèches!";
    const char search[] = "archi";
    const char replace[] = "super";

    searchAndReplace(text, search, replace);
    return 0;
}
```
Sortie prévue:
```
Les chaussettes de l'superduchesse, sont-elles sèches? Super-sèches!
```

## Deep Dive | Plongée en profondeur
Historiquement, la recherche et le remplacement sont des fonctionnalités clés des éditeurs de texte depuis les années 1970. Des outils comme `sed` en Unix ou `Find and Replace` dans les éditeurs modernes montrent leur importance.

Il existe plusieurs alternatives en C: fonctions intégrées dans des bibliothèques, écriture de fonctions personnalisées, ou utilisation d’expressions régulières avec la bibliothèque `regex.h`, mais cela peut être plus complexe pour des besoins simples.

En matière d’implémentation, les détails cruciaux incluent la gestion de la taille des buffers pour éviter les débordements, ainsi que l’utilisation efficace des pointeurs pour traiter les chaînes de caractères en place sans copie excessive des données.

## See Also | À voir aussi
- [C Standard Library reference - string.h](https://en.cppreference.com/w/c/string/byte)
- [GNU sed](https://www.gnu.org/software/sed/manual/sed.html)
- [Regular Expressions in C - regex.h](https://pubs.opengroup.org/onlinepubs/7908799/xsh/regex.h.html)