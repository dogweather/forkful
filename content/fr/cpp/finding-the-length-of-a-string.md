---
title:                "C++: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation. Cela peut être utile pour des opérations telles que la vérification de la validité des entrées de l'utilisateur ou la manipulation de données de texte. Dans cet article, nous allons expliquer comment trouver la longueur d'une chaîne de caractères en utilisant le langage de programmation C++.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en C++, nous allons utiliser la fonction `strlen()` de la bibliothèque standard `cstring`. Cette fonction prend en paramètre une chaîne de caractères et renvoie la longueur de celle-ci en tant que valeur entière. Voici un exemple de code pour trouver la longueur d'une chaîne de caractères :

```C++
#include <cstring>
#include <iostream>

using namespace std;

int main(){
  char chaine[] = "Bonjour!";
  int longueur = strlen(chaine);
  cout << "La longueur de la chaine de caracteres est: " << longueur << endl;
  return 0;
}
```

Dans cet exemple, nous avons créé une variable `chaine` qui contient notre chaîne de caractères et nous avons utilisé la fonction `strlen()` pour trouver sa longueur et la stocker dans une variable `longueur`. Puis, nous avons utilisé `cout` pour afficher la longueur de la chaîne à l'écran.

Voici une sortie possible pour cet exemple :

```C++
La longueur de la chaine de caracteres est: 8
```

## Deep Dive

La fonction `strlen()` parcourt la chaîne de caractères fournie en paramètre en comptant le nombre d'octets jusqu'à ce qu'elle atteigne le caractère null (`'\0'`). Cela signifie que la chaîne de caractères doit se terminer par un caractère null pour que la fonction puisse renvoyer la longueur correcte.

De plus, il est important de noter que la longueur retournée par la fonction `strlen()` ne prend pas en compte le caractère null. Par conséquent, si vous voulez inclure le caractère null dans votre calcul, vous devrez ajouter 1 à la longueur.

## Voir aussi

- [Documentation pour la fonction `strlen()` en C++](https://en.cppreference.com/w/cpp/string/byte/strlen)
- [Guide du langage C++ pour débutants](https://www.programiz.com/cpp-programming)
- [Exemples de chaînes de caractères en C++](https://www.codegrepper.com/code-examples/cpp/c%2B%2B+string+examples)