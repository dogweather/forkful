---
title:    "C: Lecture des arguments de ligne de commande"
keywords: ["C"]
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes nouveau dans le monde de la programmation en C, vous pourriez vous demander pourquoi vous devriez vous intéresser aux arguments de ligne de commande. La réponse est simple: la lecture et la manipulation de ces arguments peuvent grandement améliorer l'efficacité et la flexibilité de vos programmes.

# Comment Faire

La lecture des arguments de ligne de commande en C peut sembler intimidante au début, mais une fois que vous comprendrez le processus, cela deviendra un outil précieux dans votre trousse de programmation. Voici un exemple de code pour vous aider à démarrer:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Voici les arguments de ligne de commande fournis:\n");

    for(int i = 1; i < argc; i++) {
        printf("%s\n", argv[i]);
    }

    return 0;
}
```

Dans cet exemple, nous utilisons deux paramètres de la fonction `main()`: `argc` qui contient le nombre total d'arguments de ligne de commande, et `argv[]` qui est un tableau contenant ces arguments. Le premier élément du tableau est toujours le nom du programme lui-même, donc nous commençons à parcourir à partir de l'indice 1 pour pouvoir afficher tous les arguments fournis par l'utilisateur.

Si vous compilez et exécutez ce programme avec différents arguments, vous verrez leur contenu s'afficher sur la ligne de commande. Par exemple, en exécutant `./commande arg1 arg2 arg3`, vous obtiendrez:

```
Voici les arguments de ligne de commande fournis:
arg1
arg2
arg3
```

# Plongée Profonde

Il est important de noter que les arguments de ligne de commande sont toujours des chaînes de caractères, même s'ils représentent des nombres ou d'autres types de données. Cela signifie que vous devrez convertir ces chaînes en types appropriés selon vos besoins.

Vous pouvez également utiliser des options de ligne de commande pour rendre votre programme encore plus flexible. Par exemple, vous pouvez utiliser des arguments commençant par un tiret pour activer certaines fonctionnalités. Par exemple, en utilisant `./commande -p`, vous pourriez activer une option pour afficher une sortie plus détaillée.

# Voir Aussi

- [La fonction `main()` en C](https://www.programiz.com/c-programming/c-main-functions)
- [Manipulation de chaînes de caractères en C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [Guide des arguments de ligne de commande en C](https://www.guru99.com/command-line-arguments-c.html)