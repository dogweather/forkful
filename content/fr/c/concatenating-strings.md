---
title:    "C: Concaténation de chaînes de caractères"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# Pourquoi
La concaténation de chaînes de caractères est un outil essentiel pour tout programmeur en C. Elle permet de combiner plusieurs chaînes de caractères en une seule, ce qui est très utile pour la création de messages, la manipulation de données et bien plus encore.

# Comment faire
La concaténation de chaînes de caractères se fait à l'aide de la fonction `strcat()`. Cette fonction prend deux paramètres : la chaîne de caractères de destination et la chaîne de caractères à ajouter. Voici un exemple de code montrant comment utiliser la fonction `strcat()` :

```C
#include <stdio.h>
#include <string.h>

int main() {
  char destination[50] = "Bonjour ";
  char ajout[20] = "mon ami !";
  strcat(destination, ajout);
  
  printf("Résultat : %s", destination);
  
  // Output: Bonjour mon ami !
  return 0;
}
```

# Plongeon en profondeur
Il est important de noter que la fonction `strcat()` modifie directement la chaîne de caractères de destination. Cela signifie qu'il faut faire attention à la taille de la chaîne de destination pour éviter une erreur de segmentation. Il existe également d'autres fonctions de concaténation, telles que `strncat()` qui limite le nombre de caractères à ajouter. De plus, la librairie standard `<string.h>` propose également des fonctions pour la manipulation de chaînes de caractères, comme `strcpy()` et `strlen()`.

# Voir aussi
- [Documentation sur la fonction strcat()](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Autres fonctions de manipulation de chaînes de caractères](https://www.tutorialspoint.com/c_standard_library/string_h.htm)