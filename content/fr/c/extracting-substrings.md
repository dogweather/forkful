---
title:    "C: Extraction de sous-chaînes"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

Dans la programmation, il est souvent nécessaire de travailler avec des chaînes de caractères. Cela peut inclure des opérations telles que la recherche, le remplacement et la suppression de parties spécifiques d'une chaîne. Pour cela, l'extraction de sous-chaînes est une technique cruciale à connaître.

# Comment faire

L'extraction de sous-chaînes consiste à extraire une portion spécifique d'une chaîne de caractères existante. Pour cela, nous avons besoin de trois paramètres : la chaîne d'origine, l'indice de départ et la longueur de la sous-chaîne désirée.

Voici un exemple de code en C montrant comment extraire une sous-chaîne :

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Bonjour le monde";
    int index = 8;
    int length = 5;

    char subString[length + 1];

    memcpy(subString, &str[index], length);
    subString[length] = '\0';

    printf("La sous-chaîne extraite est : %s\n", subString);

    return 0;
}
```

Dans cet exemple, nous déclarons une chaîne de caractères initiale "Bonjour le monde", et nous spécifions l'indice à partir duquel extraire la sous-chaîne (dans ce cas, l'indice 8 correspondant à "le"). Nous spécifions également la longueur de la sous-chaîne souhaitée (5 caractères). Ensuite, nous utilisons la fonction "memcpy" pour copier la sous-chaîne à partir de l'indice spécifié, et nous terminons en ajoutant le caractère de fin de chaîne ('\0').

Lorsque nous exécutons ce code, nous obtenons l'output suivant :

```
La sous-chaîne extraite est : le mo
```

# Plongée en profondeur

Il existe d'autres façons de réaliser l'extraction de sous-chaînes en utilisant différentes fonctions de la bibliothèque standard de C, telles que "strncpy", "strndup" ou "sscanf". Chacune de ces fonctions a ses propres particularités et peut être utile en fonction du contexte.

Il est également important de noter que l'extraction de sous-chaînes fonctionne également avec des tableaux de caractères, pas seulement des chaînes de caractères. Cela peut être utile lorsqu'on travaille avec des données binaires.

# Voir aussi

- [Tutoriel C : Chaînes de caractères](https://www.codingame.com/playgrounds/4186/le-langage-c-pour-les-zeroes/les-chaines-de-caracteres)
- [Documentation sur les fonctions de string.h en C](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Guide pratique de l'extraction et de la manipulation de sous-chaînes en C](https://www.andatsoft.fr/programmation/guide_pratique_extraction_sous_chaine_manipulation_en_c.php)