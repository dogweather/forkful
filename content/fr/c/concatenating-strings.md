---
title:                "C: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une tâche courante en programmation. Elle consiste à fusionner deux ou plusieurs chaînes de caractères pour former une seule chaîne. Cette opération peut être utile dans de nombreux cas, tels que l'affichage de messages, la manipulation de données ou la construction de requêtes.

## Comment faire

En langage C, la concaténation de chaînes de caractères s'effectue à l'aide de la fonction `strcat()` de la bibliothèque standard `string.h`. Pour utiliser cette fonction, il suffit de lui fournir les deux chaînes à concaténer en paramètres. Voici un exemple de code qui concatène deux chaînes et affiche le résultat :

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[50] = "Hello";
    char str2[50] = " world!";
    
    strcat(str1, str2);
    
    printf("%s", str1);
    
    return 0;
}
```

Lorsque vous exécutez ce code, vous devriez obtenir comme sortie `Hello world!`. Si vous souhaitez concaténer plus de deux chaînes, il suffit de répéter l'opération autant de fois que nécessaire.

## Plongée en profondeur

La fonction `strcat()` concatène les chaînes de caractères en les fusionnant l'une après l'autre dans la première chaîne. Ainsi, pour concaténer une chaîne `str1` avec une autre chaîne `str2`, `str1` doit avoir suffisamment d'espace pour contenir toutes les caractères de `str2`. Sinon, des débordements de mémoire peuvent se produire, entraînant des erreurs de segmentation.

Pour éviter ce type d'erreur, il est recommandé d'utiliser la fonction `strncat()` qui, en plus des deux chaînes à concaténer, prend en paramètre un entier représentant le nombre maximum de caractères à concaténer.

En outre, il est important de noter que la fonction `strcat()` modifie la première chaîne en place, c'est-à-dire que son contenu est modifié directement en mémoire. Si vous souhaitez conserver la première chaîne telle quelle, il est conseillé de copier son contenu dans une autre variable avant de concaténer les chaînes.

## Voir aussi
- [Fonction strcat() de la bibliothèque string.h (en anglais)](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Guide de référence sur la fonction strcat() (en anglais)](https://docs.microsoft.com/en-us/cpp/c-language/strcat-strncat-wcscat-wcsncat?view=vs-2019)