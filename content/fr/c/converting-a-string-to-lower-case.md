---
title:    "C: Convertir une chaîne en minuscules"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il existe plusieurs raisons pour lesquelles convertir une chaîne de caractères en minuscules peut être utile. Tout d'abord, cela peut rendre les chaînes de caractères plus cohérentes et plus faciles à comparer. Deuxièmement, cela peut être nécessaire pour traiter des données provenant de différents sources qui peuvent utiliser des conventions différentes pour la casse des lettres.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en utilisant le langage de programmation C, vous pouvez utiliser la fonction `tolower()` de la bibliothèque standard. Cette fonction prend en paramètre un caractère et le convertit en sa forme minuscule. Voici un exemple de code montrant comment utiliser cette fonction :

```
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[] = "HELLO WORLD";
  int i;

  for(i=0; str[i]; i++){
    str[i] = tolower(str[i]);
  }
  printf("%s", str);
  return 0;
}
```

La sortie de ce code sera "hello world".

## Plongée profonde

Il est important de noter que la fonction `tolower()` ne fonctionne que sur des caractères individuels et ne peut pas être utilisée pour convertir une chaîne de caractères entière. Pour convertir une chaîne de caractères en minuscules, il est donc nécessaire d'utiliser une boucle pour parcourir tous les caractères et les convertir un par un.

Il existe également une autre fonction, `strlwr()`, qui peut être utilisée pour convertir une chaîne de caractères en minuscules en une seule ligne de code. Cependant, cette fonction n'est pas toujours disponible sur toutes les plateformes et n'est pas incluse dans la bibliothèque standard.

Il est également important de noter que la conversion en minuscules dépend du jeu de caractères utilisé. Par exemple, la conversion en minuscules d'un caractère accentué en français peut être différente de celle d'un caractère accentué en anglais.

## Voir aussi

- [Tutorialspoint C library - tolower()](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [GeeksforGeeks C library - tolower()](https://www.geeksforgeeks.org/tolower-function-in-c/)
- [Manuel du langage C - strlwr()](https://man7.org/linux/man-pages/man3/strlwr.3.html)