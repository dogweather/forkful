---
title:    "C: Concaténation de chaînes de caractères"
keywords: ["C"]
---

{{< edit_this_page >}}

##Pourquoi:
La concaténation de chaînes de caractères peut être utile lorsque vous avez besoin de combiner plusieurs chaînes pour créer une seule chaîne plus longue. Cela peut être particulièrement utile lors de l'affichage de messages ou de données dynamiques.

##Comment faire:
Pour concaténer des chaînes de caractères en C, vous devez utiliser la fonction `strcat ()`. Voici un exemple de code montrant comment concaténer deux chaînes et l'afficher ensuite:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[20] = "Bonjour ";
    char str2[20] = "tout le monde!";
    strcat(str1, str2);
    printf("%s", str1);
    return 0;
}
```
Résultat:
```
Bonjour tout le monde!
```
Comme vous pouvez le voir, la fonction `strcat ()` a combiné les deux chaînes en une seule.

##Plongée en profondeur:
Il est important de noter que lors de la concaténation de chaînes en C, la première chaîne doit avoir suffisamment d'espace pour contenir la deuxième chaîne ajoutée. Si la première chaîne n'est pas assez grande, des erreurs peuvent se produire ou des résultats inattendus peuvent être affichés. Il est également possible de concaténer plusieurs chaînes en utilisant plusieurs fois la fonction `strcat ()`.

##Voir aussi:
- [Documentation sur la fonction strcat en C](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Exemples de concaténation de chaînes en C](https://www.geeksforgeeks.org/concatenation-of-two-strings-in-c/)
- [Tutorial vidéo sur la concaténation de chaînes en C](https://www.youtube.com/watch?v=MI_YcBKiQk0)