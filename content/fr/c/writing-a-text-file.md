---
title:                "Écriture d'un fichier texte"
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Écrire dans un fichier texte c’est permettre à votre programme C de sauvegarder des données de manière persistante. Les développeurs utilisent cela pour log information, sauvegarder des configurations ou exporter des données à utiliser ailleurs.

## Comment faire :

```c
#include <stdio.h>

int main() {
    FILE *fichier;
    
    // Ouvrir le fichier en mode écriture
    fichier = fopen("sortie.txt", "w");
    
    if (fichier == NULL) {
        printf("Erreur lors de l'ouverture du fichier.\n");
        return 1;
    }
    
    // Écrire du texte dans le fichier
    fprintf(fichier, "Bonjour, monde!\n");
    fprintf(fichier, "Ça va ?");
    
    // Fermer le fichier
    fclose(fichier);
    
    printf("Écriture terminée.\n");
    return 0;
}
```

Sortie attendue : "Écriture terminée."

Après exécution, vous aurez un fichier `sortie.txt` contenant le texte.

## Deep Dive

Historiquement, C introduit des manipulations de fichiers avec `stdio.h`. C'était révolutionnaire pour l'encapsulation de l'I/O en utilisant des fichiers. Alternatives incluent `write` de `unistd.h` sous Unix, ou des libs modernes comme `libuv`. Le `FILE` encapsule un stream de fichiers, et `fopen`, `fprintf`, et `fclose` gèrent respectivement l'ouverture, l'écriture, et la fermeture.

## Voir Aussi

- [C Standard Library - stdio.h](https://en.cppreference.com/w/c/io)
- [C File I/O Guide](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [GNU C Library Manual](https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html)
