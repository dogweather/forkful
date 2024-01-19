---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?

Les expressions régulières sont des séquences de caractères permettant de trouver des motifs dans du texte. Les programmeurs les utilisent pour parcourir les chaînes de caractères de façon rapide et efficace.

## Comment faire :

Voici comment on pourrait utiliser les expressions régulières en C. 

```C
#include <regex.h> 

int main() {
    regex_t regex;
    int return_val;
    return_val = regcomp(&regex, "c", 0); 
    return_val = regexec(&regex, "Coding", 0, NULL, 0);  
    if (return_val == 0) {
        printf("Match");
    } 
    else {
        printf("No Match");
    }
    return 0;
}
```
La sortie de ce programme serait `Match` car "c" est bien présent dans "Coding".

## Immersion :

Les expressions régulières existent depuis les années 50 et ont été intégrées dans divers langages de programmation. En C, le traitement d'expressions régulières n'est pas intégré de manière native, mais est supporté au travers de la bibliothèque `<regex.h>`. 

Comme alternative, les programmeurs peuvent utiliser `strstr` pour la recherche de sous-chaînes, mais cela n'est pas aussi flexible que les expressions régulières. 

Le traitement des expressions régulières en C est basé sur l'algorithme de Thompson, qui convertit l'expression régulière en un automate non déterministe (NFA) pour effectuer la recherche. 

## A voir également :

Pour plus d'informations sur les expressions régulières, consultez ces liens :

1. Documentation GNU sur regex.h : https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
2. Cours sur les expressions régulières : https://www.coursera.org/lecture/data-structures-optimizing-efficiency/lecture-2-regular-expressions-p1mtK