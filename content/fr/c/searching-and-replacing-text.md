---
title:                "C: Rechercher et remplacer du texte"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans la programmation en C. Cette technique permet de modifier ou de remplacer rapidement du texte dans un fichier, ce qui peut faire gagner beaucoup de temps lors de la mise à jour du code.

## Comment faire

Pour effectuer une recherche et un remplacement de texte en C, il est nécessaire d'utiliser la fonction `strstr()`. Cette fonction prend en paramètre une chaîne de caractères à rechercher ainsi qu'une chaîne de remplacement. Elle renvoie un pointeur sur la première occurrence de la chaîne recherchée et remplace toutes les occurrences dans le texte d'origine. Voici un exemple de code :

```
#include <stdio.h>
#include <string.h>

int main()
{
    char str[50] = "Bonjour tout le monde !";
    char *ptr;
    
    printf("Texte d'origine : %s\n", str);
    
    // Recherche et remplacement
    ptr = strstr(str, "tout le monde");
    if(ptr != NULL)
    {
        strcpy(ptr, "amis");
    }
    
    printf("Nouveau texte : %s\n", str);
    
    return 0;
}
```

La sortie de ce code serait :

```
Texte d'origine : Bonjour tout le monde !
Nouveau texte : Bonjour amis !
```

## Plongée en profondeur

En plus de la fonction `strstr()`, il existe d'autres fonctions utiles pour la recherche et le remplacement de texte en C, telles que `strchr()` et `strrchr()`. Il est également possible de spécifier une limite dans la fonction `strstr()`, pour ne pas remplacer toutes les occurrences de la chaîne recherchée.

Il est important de noter que ces fonctions ne fonctionnent que pour des chaînes de caractères, et non pour des fichiers entiers. Pour traiter des fichiers, il faut lire le contenu du fichier, utiliser les fonctions de recherche et de remplacement, puis réécrire le contenu modifié dans le fichier.

## Voir aussi

- [Documentation officielle de la fonction strstr()](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Guide pour la manipulation de fichiers en C](https://www.tutorialspoint.com/c_standard_library/c_function_fread.htm)
- [Exemples de code pour la recherche et le remplacement en C](https://www.geeksforgeeks.org/c-program-replace-word-text-another-given-word/)