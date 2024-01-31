---
title:                "Suppression de caractères correspondant à un motif"
date:                  2024-01-20T17:41:38.623870-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suppression de caractères correspondant à un motif"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et pourquoi ?)
Supprimer des caractères selon un motif, c'est filtrer du texte. Les programmeurs le font pour nettoyer des données, valider des entrées ou manipuler des chaînes pour des besoins spécifiques.

## How to: (Comment faire :)
```C
#include <stdio.h>
#include <string.h>

void delete_matching_chars(char *str, const char *pattern) {
    char *pr = str, *pw = str; // pointeurs de lecture et d'écriture
    while (*pr) {
        const char *temp = pattern;
        int match = 0;
        while (*temp) {
            if (*pr == *temp++) {
                match = 1;
                break;
            }
        }
        if (!match) {
            *pw++ = *pr;
        }
        pr++;
    }
    *pw = '\0';
}

int main() {
    char str[] = "Bonjour, c'est un exemple!";
    delete_matching_chars(str, "aeiou");
    printf("Après suppression: %s\n", str); // Affiche: Bnjr, c'st n xmpl!
    return 0;
}
```

## Deep Dive (Plongée profonde)
Historiquement, supprimer des caractères selon un motif est fondamental en programmation car cela aide à traiter le texte de manière précise. Des alternatives incluent des fonctions standard comme `strtok`, `strspn`, `strcspn`, et des expressions régulières (regex), disponibles dans des bibliothèques comme `regex.h`. Pour performance et personnalisation, une implémentation manuelle, comme celle ci-dessus, est souvent préférable, surtout si les fonctions standard ne répondent pas aux besoins spécifiques.

## See Also (À voir aussi)
- Documentation de la bibliothèque standard C : https://en.cppreference.com/w/c/string/byte
- Guide des expressions régulières C : https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
- Tutoriels sur la manipulation de chaînes en C : https://www.tutorialspoint.com/cprogramming/c_strings.htm
