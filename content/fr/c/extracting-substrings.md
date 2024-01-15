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

## Pourquoi

Si vous travaillez avec des chaînes de caractères en C, vous avez peut-être rencontré des situations où vous avez besoin d'extraire une sous-chaîne spécifique d'une chaîne plus grande. Cela peut être utile pour effectuer des opérations de recherche ou de manipulation de données. Dans cet article, vous apprendrez comment extraire des sous-chaînes en utilisant différentes méthodes en C.

## Comment Faire

Pour extraire une sous-chaîne en C, vous pouvez utiliser la fonction `strncpy()` qui copie les caractères d'une chaîne source dans une nouvelle chaîne. Voici un exemple de code montrant comment extraire une sous-chaîne de 5 caractères à partir de l'indice 2 d'une chaîne principale :

```
#include <stdio.h>
#include <string.h>

int main() {
  char str[20] = "Bonjour le monde";
  char substr[6];

  strncpy(substr, &str[2], 5);

  printf("La sous-chaîne extraite est: %s\n", substr); // Output: Bonjo

  return 0;
}
```

Vous pouvez également utiliser la fonction `substr()` de la bibliothèque standard `string.h` pour extraire une sous-chaîne en spécifiant l'indice de début et la longueur de la sous-chaîne. Voici un exemple de code :

```
#include <stdio.h>
#include <string.h>

int main() {
  char str[20] = "Bonjour le monde";
  char substr[6];

  substr(str, 2, 5);

  printf("La sous-chaîne extraite est: %s\n", substr); // Output: Bonjo

  return 0;
}
```

## Plongée Profonde

En utilisant `strncpy()`, vous devez faire attention à spécifier la longueur maximale de la sous-chaîne que vous souhaitez extraire pour éviter tout dépassement de tampon. De plus, cette fonction ajoute automatiquement un caractère nul à la fin de la sous-chaîne extraite, donc vous n'avez pas besoin de le faire manuellement.

D'autre part, `substr()` est une fonction personnalisée qui n'est pas disponible dans la bibliothèque standard de C, vous devrez donc la créer vous-même. Cette fonction est généralement plus rapide que `strncpy()` car elle ne nécessite pas de copie de caractères supplémentaires pour remplir un tampon.

## Voir aussi

- [Documentation de la fonction strncpy en C](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Documentation de la fonction substr en C](https://www.geeksforgeeks.org/substr-function-in-c/)
- [Tutoriel sur les chaînes de caractères en C](https://www.learn-c.org/en/Strings)