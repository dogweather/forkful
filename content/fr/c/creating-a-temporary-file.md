---
title:                "C: Création d'un fichier temporaire"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi 

La création de fichiers temporaires est une technique couramment utilisée en programmation C pour stocker des données temporaires en mémoire. Cela peut être utile pour des tâches telles que la manipulation de fichiers, le stockage de données en cas de panne de système ou l'exécution de programmes nécessitant des fichiers temporaires.

# Comment faire

Pour créer un fichier temporaire en C, nous pouvons utiliser la fonction `tmpfile ()` qui est disponible dans la bibliothèque standard `stdio.h`. Cette fonction prend en paramètres le nom du fichier et le mode d'ouverture, et retourne un pointeur vers le fichier temporaire créé.

```
#include <stdio.h>

int main () {
    FILE *fp;
    char data[50] = "Hello world!";

    fp = tmpfile();
    if (fp == NULL) {
        perror("Impossible de créer un fichier temporaire");
        return 1;
    }

    fputs(data, fp);
    fclose(fp);
    
    return 0;
}
```

Dans cet exemple, nous avons créé un fichier temporaire et y avons écrit la chaîne "Hello world!". Ensuite, nous avons fermé le fichier en appelant la fonction `fclose()`.

Lorsque vous utilisez des fichiers temporaires, il est important de les supprimer après utilisation pour éviter l'encombrement de votre système avec des fichiers inutiles. Pour ce faire, vous pouvez utiliser la fonction `remove()` avec le nom du fichier temporaire en paramètre.

```
#include <stdio.h>

int main () {
    FILE *fp;
    char data[50] = "Bonjour le monde!";

    fp = tmpfile ();
    if (fp == NULL) {
        perror("Impossible de créer un fichier temporaire");
        return 1;
    }

    fputs(data, fp);
    fclose(fp);
    remove("tmpfile");
    
    return 0;
}
```

# Plongée en profondeur

Outre la fonction `tmpfile ()`, il existe également la fonction `tmpnam ()` qui peut être utilisée pour générer un nom de fichier temporaire unique. Cette fonction retourne une chaîne de caractères contenant le chemin du fichier temporaire.

```
#include <stdio.h>

int main () {
    char temp_file_name[L_tmpnam];
    tmpnam(temp_file_name);
    printf("Le nom du fichier temporaire est %s\n", temp_file_name);

    return 0;
}
```

# Voir aussi

- [Guide de programmation C en Français](https://www.programiz.com/c-programming)
- [Documentation sur les fonctions de la bibliothèque standard de C](https://www.cplusplus.com/reference/cstdio/)
- [Guide complet sur la manipulation de fichiers en C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)