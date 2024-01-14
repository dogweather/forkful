---
title:                "C: Trouver la longueur d'une chaîne"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Trouver la longueur d'une chaîne de caractères est une compétence de base indispensable en programmation. Cela permet de manipuler efficacement des chaînes de caractères et de les utiliser dans des algorithmes complexes.

# Comment faire

Pour trouver la longueur d'une chaîne de caractères en langage C, nous utilisons la fonction `strlen()` de la bibliothèque standard `string.h`. Cette fonction prend en paramètre une chaîne de caractères et renvoie sa longueur. Voici un exemple de code avec une chaîne de caractères définie comme une variable :

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Bonjour!";
  printf("La longueur de la chaîne '%s' est %d.\n", str, strlen(str));
  return 0;
}
```

Lorsque vous exécutez ce code, vous obtiendrez la sortie suivante :

```
La longueur de la chaîne 'Bonjour!' est 8.
```

Il est important de noter que la longueur calculée inclut le caractère de fin de chaîne `\0`.

# Plongée en profondeur

La fonction `strlen()` utilise une boucle pour parcourir la chaîne de caractères et compter le nombre de caractères avant le caractère `\0` de fin de chaîne. Cela signifie que cette fonction prend un temps proportionnel à la longueur de la chaîne pour s'exécuter.

Il existe d'autres façons de trouver la longueur d'une chaîne de caractères en C, comme en utilisant une boucle et en incrémentant un compteur à chaque caractère jusqu'à atteindre le caractère `\0`. Cependant, la fonction `strlen()` est plus optimisée et plus rapide à utiliser.

# Voir aussi

- Documentation sur la fonction `strlen()` : https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm
- Comparaison entre `strlen()` et une boucle : http://tech.pro/tutorial/1269/c/how-to-get-string-length-the-fast-way-using-strlen
- Vidéo explicative sur la fonction `strlen()` : https://www.youtube.com/watch?v=NqKryAyUDzE