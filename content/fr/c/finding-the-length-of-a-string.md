---
title:                "C: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche très courante en programmation C. Cela peut semer la confusion chez les débutants, mais c'est une compétence essentielle à maîtriser. En comprenant comment calculer la longueur d'une chaîne de caractères, vous serez en mesure de développer des applications plus avancées et complexes.

# Comment faire

La méthode la plus courante pour trouver la longueur d'une chaîne de caractères en programmation C est d'utiliser la fonction `strlen()`. Cette fonction prend une chaîne de caractères en tant que paramètre et renvoie le nombre de caractères dans cette chaîne. Voici un exemple de code:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[20] = "Bonjour";
    int len = strlen(str);
    printf("La longueur de la chaine est: %d", len);
    return 0;
}
```

Lorsque vous exécutez ce code, vous devriez obtenir une sortie qui ressemble à ceci:

```
La longueur de la chaine est: 7
```

La fonction `strlen()` ne compte pas le caractère nul de fin de chaîne, donc dans notre exemple, la dernière lettre "r" n'est pas incluse dans le résultat.

Il est également important de noter que la fonction `strlen()` ne fonctionne que pour les chaînes de caractères. Si vous essayez de l'utiliser sur un pointeur ou un tableau de caractères, vous risquez d'obtenir un résultat inattendu.

# Plongée en profondeur

La fonction `strlen()` utilise un pointeur pour parcourir la chaîne de caractères et compter le nombre de caractères jusqu'à ce qu'elle atteigne le caractère nul. Cela se fait en utilisant une boucle while qui vérifie à chaque itération si le caractère actuel est le caractère nul. Si ce n'est pas le cas, le pointeur est déplacé vers le caractère suivant jusqu'à ce qu'il atteigne le caractère nul.

De plus, il est important de noter que la fonction `strlen()` est définie dans l'en-tête `string.h`. Cela signifie que vous devez inclure cet en-tête dans votre code si vous voulez utiliser cette fonction.

# Voir aussi

Voici quelques liens utiles pour en savoir plus sur la fonction `strlen()` et la manipulation des chaînes de caractères en programmation C:

- [Documentation de la fonction strlen()](https://www.tutorialspoint.com/ansi_c/c_strlen.htm)
- [Manipulation de chaînes de caractères en C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Les fonctions de la bibliothèque standard en C](https://www.tutorialspoint.com/c_standard_library/index.htm)