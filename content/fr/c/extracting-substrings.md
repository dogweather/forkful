---
title:                "Extraction de sous-chaînes"
html_title:           "C: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi: 
Les programmeurs utilisent l'extraction de sous-chaines (ou sous-chaînes) lorsqu'ils ont besoin d'extraire une partie spécifique d'une chaîne de caractères. Cette partie peut être un mot, un caractère ou même un ensemble de caractères. Cela permet aux programmeurs de manipuler des chaînes de manière plus efficace et de les utiliser dans différentes opérations.

## Comment faire: 
Pour extraire une sous-chaîne en C, vous pouvez utiliser la fonction `strncpy` qui copie un nombre spécifié de caractères d'une chaîne source vers une chaîne de destination. Exemple: 
```C 
char str1[20] = "Bonjour le monde";
char str2[20];

strncpy(str2, str1, 7); //Copiez les 7 premiers caractères de str1 dans str2
printf("%s", str2); //Résultat: "Bonjour"
```

Vous pouvez également utiliser la fonction `substr` de la bibliothèque `string.h`, qui prend en paramètre la chaîne d'origine, l'index de départ et le nombre de caractères à extraire. Exemple: 
```C 
#include <string.h>

char str1[] = "Bonjour le monde";
char str2[20];

substr(str1, 8, 3, str2); //Copiez 3 caractères à partir du 8ème index de str1 dans str2
printf("%s", str2); //Résultat: "mon"
```

## Plongée en profondeur: 
L'extraction de sous-chaînes est une fonctionnalité courante dans la plupart des langages de programmation. Elle a été introduite dans les premières versions de C et a depuis été utilisée dans de nombreuses applications. Cependant, certaines bibliothèques tierces, telles que `strtok` peuvent également être utilisées pour extraire des sous-chaînes en C.

L'un des principaux avantages de l'extraction de sous-chaînes est qu'elle permet aux programmeurs de manipuler efficacement les chaînes de caractères sans avoir à créer de nouvelles variables. Cela peut être particulièrement utile lors du traitement de données d'entrée ou de sortie.

Du côté de l'implémentation, la fonction `strncpy` peut être sujette à des erreurs si le nombre de caractères spécifié n'est pas correct ou si la chaîne de destination n'est pas correctement définie. Il est donc important de faire attention à ces détails lors de l'utilisation de cette fonction.

## Voir aussi: 
- [Documentation complète du langage C](https://en.cppreference.com/w/c)
- [Tutoriel sur la manipulation de chaînes de caractères en C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Bibliothèque string.h en détail](https://www.programiz.com/c-programming/library-function/string.h)