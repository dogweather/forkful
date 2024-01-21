---
title:                "Création d'un fichier temporaire"
date:                  2024-01-20T17:39:49.774158-07:00
model:                 gpt-4-1106-preview
simple_title:         "Création d'un fichier temporaire"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Créer un fichier temporaire, c'est comme prendre des notes sur un post-it. C’est temporaire, utile pour ne pas perdre des données pendant que le programme tourne. Les programmeurs utilisent des fichiers temporaires pour stocker des données sans toucher au système de fichiers permanent.

## How to:
En C, la bibliothèque standard offre des fonctions pour créer des fichiers temporaires. Voilà comment on fait:

```C
#include <stdio.h>

int main() {
    // Crée un fichier temporaire qui se ferme et se supprime automatiquement
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Erreur de création du fichier temporaire");
        return 1;
    }

    // Utiliser fprintf ou fputs pour écrire dans le fichier
    fputs("Bonjour, fichier temporaire!", temp);

    // Déplace le pointeur de fichier au début pour la lecture
    rewind(temp);

    // Lire et afficher le contenu
    char buffer[100];
    if (fgets(buffer, sizeof(buffer), temp) == NULL) {
        perror("Erreur de lecture du fichier temporaire");
        return 1;
    }
    printf("Contenu: %s", buffer);

    // Le fichier se ferme et se supprime à la fermeture du programme, ou via fclose
    fclose(temp);

    return 0;
}
```

Sortie (output):
```
Contenu: Bonjour, fichier temporaire!
```

## Deep Dive
Créer des fichiers temporaires est courant depuis les premiers jours de la programmation en C. La fonction `tmpfile()` est standardisée par le C89. Des alternatives, comme `mkstemp()`, existent mais `tmpfile()` est simple et sûr car elle évite les conflits de nom et les problèmes de sécurité. Implementation détail: `tmpfile()` crée le fichier temporaire dans un dossier approprié et gère son cycle de vie.

La fonction `mkstemp()` crée un fichier temporaire avec un nom unique via un template de nom de fichier, qui doit inclure le pattern "XXXXXX" que `mkstemp()` remplacera par une chaîne de caractères aléatoire pour créer un nom de fichier unique.

## See Also
- La documentation de `tmpfile(3)` et `mkstemp(3)` sur le manuel Linux en ligne ([man7.org](https://man7.org/linux/man-pages/man3/tmpfile.3.html)).
- Tutoriel sur la gestion des fichiers en C ([tutorialspoint.com](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)).
- Plus d'informations sur les fichiers temporaires en sécurité informatique ([owasp.org](https://owasp.org/www-community/vulnerabilities/Insecure_Temporary_File)).