---
title:                "C: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de convertir une chaîne de caractères en lettres minuscules. Cela peut être utile pour comparer des chaînes de manière insensible à la casse ou pour l'affichage de données dans un format uniforme. Dans cet article, nous allons plonger dans la façon de le faire en utilisant le langage de programmation C.

## Comment faire

Tout d'abord, nous devons comprendre comment les caractères sont représentés en C. Les caractères sont stockés comme des nombres entiers, avec chaque caractère ayant une valeur spécifique. Les lettres minuscules ont des valeurs plus élevées que les lettres majuscules, ce qui signifie que pour convertir une lettre majuscule en minuscule, nous devons simplement ajouter un nombre spécifique à sa valeur. Par exemple, pour convertir la lettre 'A' en 'a', nous devons ajouter 32 à sa valeur. Voici un exemple de code en utilisant cette méthode :

```C
#include <stdio.h>

int main() {

  char lettre = 'A';
  lettre = lettre + 32;
  printf("%c", lettre);
  // Output : a

  return 0;
}
```

Maintenant que nous comprenons la méthodologie de base, nous pouvons écrire une fonction qui prend une chaîne de caractères et la convertit en lettres minuscules en utilisant une boucle pour parcourir chaque caractère et en utilisant la méthode mentionnée ci-dessus pour chaque lettre. Voici un exemple complet de code :

```C
#include <stdio.h>
#include <string.h>

void lowerCase(char str[]) {

  int i;

  for (i = 0; str[i] != '\0'; i++) {
    if (str[i] >= 'A' && str[i] <= 'Z') {
      str[i] = str[i] + 32;
    }
  }

}

int main() {

  char str[] = "Chaîne de Caractères";
  printf("Avant la conversion : %s \n", str);

  lowerCase(str);
  printf("Après la conversion : %s \n", str);
  // Output : Après la conversion : chaîne de caractères

  return 0;
}
```

## Plongée profonde

Il y a une autre façon de convertir une chaîne de caractères en lettres minuscules en utilisant la fonction `tolower()` définie dans la bibliothèque `ctype.h`. Cette fonction prend un caractère en argument et renvoie sa version en minuscules. En utilisant un pointeur, nous pouvons appliquer cette fonction à chaque caractère de la chaîne, comme suit :

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

void lowerCase(char str[]) {

  char *ptr;
  for (ptr = str; *ptr != '\0'; ptr++) {
    *ptr = tolower(*ptr);
  }

}

int main() {

  char str[] = "Chaîne de Caractères";
  printf("Avant la conversion : %s \n", str);

  lowerCase(str);
  printf("Après la conversion : %s \n", str);
  // Output : Après la conversion : chaîne de caractères

  return 0;
}
```

Cette seconde méthode peut sembler plus élégante et concise, mais elle peut être moins efficace en termes de performances. Il est donc important de peser le pour et le contre et de choisir la méthode qui convient le mieux à votre programme.

## Voir aussi

- [Documentation sur la fonction `tolower()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Tutoriel sur les chaînes de caractères en langage C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Exemples de code pour la manipulation de chaînes de caractères en C](https://www.programiz.com/c-programming/examples/string)