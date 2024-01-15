---
title:                "Écrire vers l'erreur standard"
html_title:           "C: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture vers l'erreur standard (ou stderr) est un moyen pratique pour afficher des informations importantes sur l'exécution de votre programme sans interrompre la sortie standard (ou stdout). Cela peut être particulièrement utile lorsque vous exécutez votre programme en arrière-plan ou lorsqu'il émet des avertissements ou des erreurs.

## Comment faire

Pour écrire vers l'erreur standard en C, vous pouvez utiliser la fonction `fprintf()` en utilisant le flux `stderr` comme premier argument. Voici un exemple :

```C
#include <stdio.h>
  
int main()
{
    int num = 5;
    fprintf(stderr, "La valeur de num est %d\n", num);
    return 0;
}
```

La sortie de ce programme sera :

```
La valeur de num est 5
```

Comme vous pouvez le constater, l'information est affichée sur stderr plutôt que sur stdout.

## Plongée profonde

La fonction `fprintf()` est très similaire à la fonction `printf()`, sauf qu'elle prend un deuxième argument pour le flux sur lequel écrire. En utilisant `stderr`, vous vous assurez que votre message sera affiché sur l'erreur standard plutôt que sur la sortie standard.

Il est également important de noter que tout comme `printf()`, `fprintf()` prend en compte des spécificateurs de format pour afficher des valeurs de types différents. Par exemple, `%d` est utilisé pour afficher des nombres entiers, tandis que `%s` est utilisé pour afficher des chaînes de caractères.

## Voir aussi

- [Documentation sur la fonction fprintf() en C](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Explication sur les spécificateurs de format en C](https://www.cprogramming.com/tutorial/printf-format-strings.html)