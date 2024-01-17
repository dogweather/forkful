---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est et pourquoi le faire?

Supprimer des caractères correspondants à un motif est une technique qui permet aux programmeurs d'éliminer rapidement des informations spécifiques d'une chaîne de caractères. Cela peut être utile dans des tâches telles que le nettoyage de données ou la validation des entrées utilisateur.

Comment faire:

Voici un exemple de code en C qui montre comment supprimer tous les espaces d'une chaîne de caractères:

```
#include <stdio.h>

int main () {
    char str[] = "Bonjour les amis !";
    int i = 0, j = 0;

    while (str[i]) {
        if (str[i] != ' ') {
            str[j++] = str[i];
        }
        i++;
    }
    str[j] = '\0';

    printf("La chaîne sans espaces est : %s\n", str);

    return 0;
}
```

En utilisant cet exemple, la sortie serait:

```
La chaîne sans espaces est : Bonjourlesamis!
```

Plongée profonde:

Cette technique existe depuis longtemps et est fréquemment utilisée en programmation. Cependant, il existe également d'autres façons de supprimer des caractères correspondants à un motif, telles que l'utilisation de fonctions de bibliothèque telles que ```strpbrk``` ou en utilisant des expressions régulières. La mise en œuvre peut également varier en fonction du langage de programmation utilisé.

Voir aussi:

- [Documentation de la fonction strpbrk en C](https://www.tutorialspoint.com/c_standard_library/c_function_strpbrk.htm)
- [Guide sur les expressions régulières en C](https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html)