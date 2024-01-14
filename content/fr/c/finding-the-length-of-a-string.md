---
title:    "C: Trouver la longueur d'une chaîne de caractères"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

Avant de plonger dans le sujet de cet article, posons-nous la question : pourquoi devrions-nous nous intéresser à trouver la longueur d'une chaîne de caractères en programmation ? La réponse est simple : en tant que programmeurs, nous travaillons souvent avec des données de type chaîne de caractères qui ont une longueur variable. Savoir comment trouver cette longueur est donc essentiel pour pouvoir manipuler efficacement ces données.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en langage C, nous pouvons utiliser la fonction prédéfinie `strlen()`. Cette fonction prend en paramètre une chaîne de caractères et renvoie sa longueur en tant que valeur entière. Voici un exemple de code montrant comment utiliser cette fonction :

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Bonjour le monde";
    int length = strlen(str);
    printf("La longueur de la chaîne \"%s\" est : %d", str, length);
    return 0;
}
```

La sortie de ce code sera :

```
La longueur de la chaîne "Bonjour le monde" est : 16
```

Dans cet exemple, nous avons d'abord déclaré une variable `str` de type `char` et lui avons assigné une chaîne de caractères. Ensuite, en utilisant `strlen()`, nous avons obtenu la longueur de la chaîne et l'avons stockée dans une autre variable `length`. Enfin, en utilisant `printf()`, nous avons affiché la chaîne et sa longueur.

Il est important de noter que la fonction `strlen()` ne compte que les caractères de la chaîne jusqu'au premier caractère null `\0`, il faut donc s'assurer que la chaîne se termine bien par ce caractère pour obtenir un résultat correct.

## Plongée en profondeur

Pour ceux qui veulent en savoir plus, la fonction `strlen()` parcourt simplement la chaîne de caractères jusqu'à rencontrer `\0` et compte le nombre de caractères rencontrés. Bien qu'elle soit très pratique à utiliser, cette fonction peut être assez inefficace si notre chaîne est très longue, car elle doit parcourir tous les caractères pour trouver la longueur.

Il est également à noter que pour les chaînes de caractères unicode, la fonction `strlen()` peut renvoyer une longueur incorrecte car elle compte chaque caractère comme un seul octet, même si certains caractères unicodes nécessitent plusieurs octets.

Pour ces raisons, certains programmeurs préfèrent écrire leur propre fonction pour trouver la longueur d'une chaîne de caractères, en utilisant par exemple un compteur de boucle. Cela peut être plus efficace pour traiter de grandes chaînes de caractères, mais nécessite un peu plus de code.

## Voir aussi

- [Documentation de la fonction `strlen()` en français](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [Différentes manières de trouver la longueur d'une chaîne en langage C](https://www.geeksforgeeks.org/strlen-vs-strlen-vs-strlen/)

N'hésitez pas à explorer différentes solutions pour trouver la longueur d'une chaîne de caractères en langage C et à choisir celle qui convient le mieux à votre projet. Bonne programmation !