---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "C: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Introduction
Salut les programmeurs ! Aujourd'hui, nous allons parler d'un sujet très important en programmation : trouver la longueur d'une chaîne de caractères en C ! C'est souvent une tâche courante dans de nombreux programmes et il est important de comprendre comment le faire correctement. Alors, arrêtons de tergiverser et plongeons directement dans le vif du sujet !

## What & Why?
Trouver la longueur d'une chaîne de caractères est simplement le fait de déterminer combien de caractères composent cette chaîne. Les programmeurs le font pour plusieurs raisons, comme par exemple pour allouer suffisamment d'espace mémoire pour stocker la chaîne, ou pour la comparer à d'autres chaînes. En résumé, c'est une étape importante pour manipuler et gérer des chaînes de caractères dans un programme.

## How to:
Pour trouver la longueur d'une chaîne de caractères en C, on utilise la fonction prédéfinie *strlen* de la bibliothèque standard *string.h*. Cette fonction prend en paramètre une chaîne de caractères et retourne un entier correspondant à sa longueur. Voici un exemple de code :

```C
#include <stdio.h>
#include <string.h>

int main() {
    char nom[] = "John";
    printf("La longueur de la chaîne '%s' est : %d\n", nom, strlen(nom));
    return 0;
}
```

Lorsque ce code est compilé et exécuté, on obtient l'output suivant :

```C
La longueur de la chaîne 'John' est : 4
```

## Deep Dive
La fonction *strlen* existe depuis longtemps en C, sa première version remonte aux années 70 lorsque le langage a été créé. Cependant, il est important de noter qu'elle n'est pas toujours la seule façon de trouver la longueur d'une chaîne de caractères. Certains programmeurs préfèrent utiliser une boucle pour parcourir la chaîne et compter le nombre de caractères, tandis que d'autres préfèrent utiliser la fonction *sizeof* pour trouver la taille de la chaîne en mémoire. Cependant, *strlen* reste la méthode la plus simple et efficace pour faire cette tâche.

Pour ceux qui sont intéressés, voici un code utilisant une boucle pour trouver la longueur d'une chaîne :

```C
#include <stdio.h>

int main() {
    char nom[] = "John";
    int longueur = 0;

    while (nom[longueur] != '\0') {
        longueur++;
    }

    printf("La longueur de la chaîne '%s' est : %d\n", nom, longueur);
    return 0;
}
```

Et voici un autre exemple utilisant la fonction *sizeof* :

```C
#include <stdio.h>

int main() {
    char nom[] = "John";
    int longueur = sizeof(nom) / sizeof(char) - 1;

    printf("La longueur de la chaîne '%s' est : %d\n", nom, longueur);
    return 0;
}
```

## See Also
Pour en savoir plus sur la fonction *strlen*, vous pouvez consulter sa documentation officielle ici : [https://www.cplusplus.com/reference/cstring/strlen/](https://www.cplusplus.com/reference/cstring/strlen/)

Vous pouvez également explorer les autres façons de trouver la longueur d'une chaîne de caractères en C en faisant vos propres recherches.

Enfin, n'hésitez pas à explorer notre plateforme pour découvrir d'autres articles sur le langage C et la programmation en général. Bonne programmation à tous !