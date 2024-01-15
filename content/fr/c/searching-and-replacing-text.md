---
title:                "Recherche et remplacement de texte"
html_title:           "C: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de remplacer du texte dans des programmes C, que ce soit pour corriger des erreurs ou pour effectuer une mise à jour. Cette fonctionnalité permet de gagner du temps et d'optimiser la qualité du code.

## Comment faire
Pour remplacer du texte dans un programme en C, il suffit d'utiliser la fonction `strstr()` qui permet de trouver une sous-chaîne dans une chaîne de caractères. Voici un exemple de code pour remplacer toutes les occurrences de "bonjour" par "hello" dans une chaîne de caractères :

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Bonjour tout le monde !";
    char *ptr = strstr(str, "bonjour"); // recherche de la sous-chaîne "bonjour"
    while (ptr != NULL) { // tant que des occurrences sont trouvées
        strncpy(ptr, "hello", 5); // remplacer la sous-chaîne par "hello"
        ptr += 5; // déplacer le pointeur de 5 caractères
        ptr = strstr(ptr, "bonjour"); // rechercher la prochaine occurrence de "bonjour"
    }
    printf("%s", str); // afficher la chaîne modifiée
    return 0;
}

// Output : Hello tout le monde !
```

## Plongée en profondeur
La fonction `strstr()` est très utile lorsqu'on souhaite rechercher et remplacer du texte. Elle renvoie un pointeur vers la première occurrence de la sous-chaîne recherchée et permet ainsi de l'utiliser pour remplacer cette sous-chaîne. Il est également possible d'utiliser d'autres fonctions telles que `strchr()` ou `strrchr()` pour trouver et remplacer du texte dans des chaînes de caractères.

## Voir aussi
- [Documentation officielle sur la fonction `strstr()` en C](https://www.cplusplus.com/reference/cstring/strstr/)
- [Tutorial sur la manipulation de chaînes de caractères en C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Exemples de code pour rechercher et remplacer du texte en C](https://www.programiz.com/c-programming/examples/replace-string)