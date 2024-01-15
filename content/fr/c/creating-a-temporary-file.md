---
title:                "Création d'un fichier temporaire"
html_title:           "C: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Les fichiers temporaires sont utiles pour stocker des informations de manière provisoire pendant l'exécution d'un programme, en particulier lorsqu'il s'agit de données volumineuses. Par exemple, un programme de traitement de données peut créer un fichier temporaire pour stocker des données intermédiaires avant de les traiter et de les supprimer une fois le processus terminé.

## How To

Creating a temporary file in C requires the use of the `tmpfile()` function, which returns a `FILE*` pointer to the newly created file. Below is a code snippet demonstrating this:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE* temp_file = tmpfile(); // create temporary file
    if (temp_file == NULL) {
        printf("Failed to create temporary file\n");
        exit(1);
    }
    // use the temporary file for storing data or performing operations
    // ...
    fclose(temp_file); // close and automatically delete the file
    return 0;
}
```

Sample output:
```
Temporary file successfully created and used for processing data
```

## Deep Dive

En plus de la fonction `tmpfile()`, il existe également d'autres fonctions pour créer et manipuler des fichiers temporaires en C. Par exemple, la fonction `tmpnam()` génère un nom unique pour un fichier temporaire et la fonction `remove()` permet de supprimer un fichier.

Il est important de noter que les fichiers temporaires sont automatiquement supprimés à la fin du programme, mais ils peuvent également être supprimés manuellement à l'aide de la fonction `remove()` si nécessaire.

## See Also

Pour en savoir plus sur les fichiers temporaires en C, vous pouvez consulter les ressources suivantes :

- Documentation officielle de la fonction `tmpfile()` : https://www.cplusplus.com/reference/cstdio/tmpfile/
- Tutoriel sur l'utilisation de fichiers temporaires en C : https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm
- Discussion sur la suppression de fichiers temporaires en C : https://stackoverflow.com/questions/1503603/creating-deleting-temporary-files-in-c