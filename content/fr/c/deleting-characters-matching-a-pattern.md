---
title:    "C: Suppression de caractères correspondant à un motif"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi
Parfois, lorsque vous travaillez avec des chaînes de caractères, vous pouvez avoir besoin de supprimer certaines caractères en fonction d'un modèle spécifique. Cela peut sembler une tâche simple, mais elle peut être assez complexe à réaliser en pratique. Dans cet article, nous allons discuter de la suppression de caractères correspondant à un modèle dans le langage de programmation C.

## Comment faire
Supposons que nous avons une chaîne de caractères "Bonjour, comment ça va ?" et que nous souhaitons supprimer tous les caractères en majuscule de cette chaîne. Voici un exemple de code en C pour le réaliser :

```
#include <stdio.h>
#include <string.h>

int main(){
    char str[] = "Bonjour, comment ça va ?";
    char result[30];
   
    int j = 0;
    int i;
    
    for(i = 0; i < strlen(str); i++){
        if(str[i] >= 'a' && str[i] <= 'z'){
            result[j] = str[i];
            j++;
        }
    }
    
    result[j] = '\0';
    
    printf("La chaîne résultante est : %s", result);
    return 0;
}
```

La sortie de ce code sera :

```
La chaîne résultante est : onjourcommentav
```

Nous avons parcouru la chaîne d'origine et copié uniquement les caractères en minuscule dans la nouvelle chaîne `result`. Enfin, nous avons ajouté un caractère de fin de chaîne à la nouvelle chaîne pour la terminer correctement.

## Deep Dive
Dans l'exemple précédent, nous avons utilisé une boucle for pour parcourir la chaîne d'origine et un tableau pour stocker les caractères de la nouvelle chaîne. Cependant, il existe d'autres moyens de réaliser la même tâche, tels que les pointeurs et les fonctions prédéfinies telles que `strchr()` et `memmove()`. De plus, la suppression de caractères peut également être réalisée en utilisant des expressions régulières. Il est essentiel de comprendre ces différentes méthodes pour choisir celle qui convient le mieux à votre programme.

## Voir aussi
- [Tutorialspoint - String Manipulation in C](https://www.tutorialspoint.com/cprogramming/c_string_manipulation.htm)
- [GeeksforGeeks - Removing leading and trailing characters from a string in C](https://www.geeksforgeeks.org/remove-leading-and-trailing-spaces-from-a-string-in-c/)
- [Cplusplus.com - String manipulation in C++](https://www.cplusplus.com/reference/string/string/)

Merci d'avoir lu cet article sur la suppression de caractères correspondant à un modèle en C. En espérant que cela vous ait été utile !