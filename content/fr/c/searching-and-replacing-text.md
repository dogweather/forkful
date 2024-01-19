---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

La recherche et le remplacement de texte est une opération qui permet de localiser une chaîne de caractères spécifique dans un texte et de la remplacer par une autre. Les programmeurs le font pour modifier les valeurs de texte, corriger les erreurs, adapter les codes à de nouvelles exigences ou réaliser des transformations de données en masse.

## Comment faire :

Voici un exemple de programme C pour rechercher et remplacer du texte. Voyons comment l'écrire :

```C
#include<stdio.h>
#include<string.h>

void chercher_remplacer(char *str, char *ancienW, char *nouveauW) {
    char buf[1024] = {0};
    int i = 0, j = 0, k = 0;
    
    while (str[k] != '\0') {
        if (strstr(&str[k], ancienW) == &str[k]) {
            strcpy(&buf[j], nouveauW);
            j += strlen(nouveauW);
            k += strlen(ancienW);
        } else
            buf[j++] = str[k++];
    }

    buf[j] = '\0';
    strcpy(str, buf);
}

int main() {
    char str[1024] = "Bonjour tout le monde. Bonjour!";
    char c1[10] = "Bonjour";
    char c2[10] = "Salut";

    chercher_remplacer(str, c1, c2);
    printf("%s", str);

    return 0;
}
```

Ceci donne le résultat suivant : `Salut tout le monde. Salut!`.

## Deep Dive

Le fait de rechercher et remplacer du texte est une pratique courante depuis les débuts de la programmation et est intrinsèquement lié au traitement de texte. En C, nous utilisons des fonctions intégrées comme `strstr` et `strcpy` de la bibliothèque `<string.h>`. 

Parmi les alternatives, on peut citer les expressions régulières, très puissantes pour la recherche de motifs dans des chaînes. Cependant, C n'a pas de support natif pour les regex. Des bibliothèques externes, comme PCRE, sont nécessaires.

Quant à l'implémentation, notre fonction `chercher_remplacer` utilise un tampon `buf` pour construire la nouvelle chaîne, copie les caractères un par un, et remplace les occurrences de l'ancien mot par le nouveau. Bien que cette approche soit générale et fonctionnelle, elle n'est pas la plus efficace en termes de performance, notamment pour les longs textes.

## Voir Aussi

- Pour une introduction à la bibliothèque de chaînes C : <https://www.learn-c.org/en/String_Manipulation>
- Pour en savoir plus sur les expressions régulières : <https://www.regular-expressions.info/>
- Documentation sur la bibliothèque PCRE : <https://www.pcre.org/>