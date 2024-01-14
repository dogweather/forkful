---
title:    "C: Extraction de sous-chaînes"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une tâche courante en programmation, surtout en langage C. Cela peut être utile pour diverses raisons, telles que la manipulation de chaînes de caractères, la recherche de motifs dans une chaîne ou encore le traitement de données provenant de fichiers externes. Peu importe la raison, savoir comment extraire des sous-chaînes est une compétence précieuse pour tout programmeur en C.

## Comment procéder

Pour extraire une sous-chaîne d'une chaîne en C, il suffit d'utiliser la fonction `strncpy`. Cette fonction copie un nombre désigné de caractères d'une chaîne source vers une chaîne destination. Pour utiliser cette fonction, voici un exemple de code :

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char chaine_source[50] = "Je suis un programmeur en C";
    char chaine_destination[20];
    
    strncpy(chaine_destination, chaine_source + 12, 9);
    
    printf("Sous-chaîne : %s", chaine_destination);
    
    return 0;
}
```

**Sortie :** Sous-chaîne : programmeur

Dans cet exemple, nous utilisons la fonction `strncpy` pour copier les 9 caractères à partir de l'indice 12 de la chaîne source dans la chaîne destination. Cette méthode est très utile pour extraire des mots ou des portions de mots à partir d'une phrase.

## Plongeons plus profondément

Il est important de noter que la fonction `strncpy` ne garantit pas que la chaîne de destination se termine par un caractère nul ('\0'). Pour éviter cela, il est recommandé de toujours ajouter le caractère nul à la fin de la sous-chaîne extraite.

De plus, si la chaîne source est plus petite que la longueur spécifiée, la fonction remplira le reste de la chaîne destination avec des caractères nuls. Cela peut être problématique si l'on souhaite obtenir une sous-chaîne de la même taille que la source. Dans ce cas, il est préférable d'utiliser la fonction `strncpy` en combinaison avec la fonction `strncat` pour concaténer les deux sous-chaînes.

Il existe également d'autres fonctions utiles pour extraire des sous-chaînes en C, telles que `strchr` pour trouver la position d'un caractère dans une chaîne et `strstr` pour trouver la position d'une sous-chaîne dans une chaîne. N'hésitez pas à vous renseigner davantage sur ces fonctions pour élargir vos compétences en matière d'extraction de sous-chaînes.

## Voir aussi

- [Documentation sur la fonction strncpy](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Documentation sur la fonction strchr](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- [Documentation sur la fonction strstr](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)