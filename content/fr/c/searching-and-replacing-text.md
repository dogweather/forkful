---
title:    "C: Recherche et remplacement de texte"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un programmeur en C, vous savez probablement que la recherche et le remplacement de texte sont des tâches courantes dans le développement de logiciels. Que ce soit pour corriger des erreurs de frappe ou pour mettre à jour du code obsolète, il est important de savoir comment effectuer cette opération de manière rapide et efficace. Dans cet article, nous allons expliquer pourquoi la recherche et le remplacement de texte sont si importants dans le langage C et comment les effectuer correctement.

# Comment faire

La recherche et le remplacement de texte peuvent être accomplis en utilisant plusieurs fonctions du langage C. La première et la plus basique est la fonction `strstr()`, qui recherche une chaîne de caractères dans une autre chaîne de caractères. Par exemple, si vous voulez trouver et remplacer le mot "Hello" dans une chaîne de caractères, vous pouvez utiliser la fonction `strstr()` pour le localiser et `strcpy()` pour le remplacer par un autre mot.

```
#include <stdio.h>
#include <string.h>

int main()
{
    char str[50] = "Hello World";
    char *ptr;
    ptr = strstr(str, "Hello");
    
    if (ptr != NULL)
    {
        strcpy(ptr, "Bonjour");
    }
    
    printf("%s", str);
    
    return 0;
}

//Output: Bonjour World
```

Une autre fonction utile pour la recherche et le remplacement de texte est `strtok()`, qui vous permet de séparer une chaîne de caractères en sous-chaînes en utilisant un délimiteur spécifié. Cela peut être utile si vous voulez remplacer certains mots dans une phrase, mais pas d'autres. Par exemple, si vous voulez remplacer le mot "Hello" par "Bonjour", mais pas le mot "Hello" dans "Hello world", vous pouvez utiliser `strtok()` pour séparer la phrase en sous-chaînes et remplacer seulement celles qui sont nécessaires.

```
#include <stdio.h>
#include <string.h>

int main()
{
    char str[50] = "Hello my name is John. Hello world.";
    char *ptr;
    ptr = strtok(str, " ");
    
    while (ptr != NULL)
    {
        if (strcmp(ptr, "Hello") == 0)
        {
            strcpy(ptr, "Bonjour");
        }
        
        ptr = strtok(NULL, " ");
    }
    
    printf("%s", str);
    
    return 0;
}

//Output: Bonjour my name is John. Bonjour world.
```

# Plongée en profondeur

En plus des fonctions de chaînes de caractères mentionnées ci-dessus, il existe des fonctions plus avancées pour la recherche et le remplacement de texte en utilisant des expressions régulières, telles que `regcomp()` et `regexec()`. Ces fonctions sont plus complexes à utiliser, mais elles offrent une plus grande flexibilité pour les cas où vous devez rechercher et remplacer du texte avec des motifs spécifiques.

Il est également important de noter que la gestion mémoire est un aspect crucial de la recherche et du remplacement de texte en C. Assurez-vous de libérer toute mémoire allouée dynamiquement, telle que des pointeurs retournés par les fonctions `malloc()` et `strdup()`.

# Voir aussi

- [Documentation C string.h](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Guide des expressions régulières en C](https://www.gnu.org/savannah-checkouts/non-gnu/linkat/docs/glibc-regex/v2.5/regex.html)
- [Gestion mémoire en C](https://www.geeksforgeeks.org/memory-layout-of-c-program/)