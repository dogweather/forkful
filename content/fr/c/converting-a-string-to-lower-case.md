---
title:    "C: Convertingir une chaîne en minuscule"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi
La conversion d'une chaîne de caractères en minuscules est une opération courante en programmation qui peut être utile pour différents cas d'utilisation. Que ce soit pour comparer des chaînes de caractères de manière insensible à la casse, ou pour afficher des données dans un format uniforme, savoir comment effectuer cette opération peut être très pratique. Dans cet article, nous allons explorer comment convertir des chaînes en minuscules en utilisant le langage de programmation C.

## Comment faire
Pour convertir une chaîne de caractères en minuscules en C, nous utiliserons la fonction `tolower()` de la bibliothèque standard `ctype.h`. Cette fonction prend en paramètre un caractère et renvoie sa version en minuscules s'il s'agit d'une lettre majuscule. Si le caractère est déjà en minuscules, il est renvoyé tel quel. Voici un exemple de code montrant comment utiliser cette fonction :

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[] = "BONJOUR";
  int i;

  for(i = 0; str[i] != '\0'; i++) {
    str[i] = tolower(str[i]);
  }

  printf("La chaîne en minuscules est : %s", str);
  return 0;
}
```

Le code ci-dessus prend une chaîne de caractères en majuscules et la convertit en minuscules en utilisant une boucle `for` et la fonction `tolower()`. Voici l'output de ce code :

```
La chaîne en minuscules est : bonjour
```

Il est également possible d'effectuer la conversion en utilisant la fonction `strlwr()` de la bibliothèque `string.h`, mais cette fonction n'est pas considérée comme standard car elle est spécifique à certaines versions de C.

## Plongée en profondeur
La conversion d'une chaîne de caractères en minuscules peut sembler simple, mais en réalité, cela peut être plus complexe qu'il n'y paraît. En effet, la façon dont les caractères sont stockés et représentés dépend du système d'exploitation et de l'encodage utilisé. Par exemple, en ASCII, il existe une correspondance directe entre les caractères en majuscules et en minuscules, tandis qu'en Unicode, il y a plusieurs équivalents en minuscules pour un caractère en majuscules donné.

Il est donc important de prendre en compte ces différences lors de la conversion de chaînes de caractères en minuscules, afin de garantir un résultat cohérent et correct.

## Voir aussi
- [Documentation de la fonction tolower() en C](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Documentation de la fonction strlwr() en C](https://www.tutorialspoint.com/c_standard_library/c_function_strlwr.htm)
- [Introduction à l'Unicode en informatique](https://www.journaldunet.fr/web-tech/dictionnaire-du-webmastering/1442498-unicode-definition-et-fonctionnement/)