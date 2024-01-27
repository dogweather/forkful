---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Écrire dans l'erreur standard permet de séparer les messages d’erreur du flux principal de données. C'est crucial pour le débogage et le diagnostic.

## How to (Comment faire) :
Utilisez `fprintf` ou `fputs` avec `stderr`. Voici un exemple:

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Ceci est une erreur!\n");
    return 0;
}

// Sortie attendue : Ceci est une erreur!
```

Autre méthode simple :

```C
#include <stdio.h>

int main() {
    fputs("Encore une erreur!\n", stderr);
    return 0;
}

// Sortie attendue : Encore une erreur!
```

## Deep Dive (Plongée en profondeur) :
Historiquement, `stderr` est séparé de `stdout` pour que les erreurs puissent être traitées différemment. Par exemple, en les redirigeant vers un fichier ou un outil de journalisation sans mélanger avec la sortie standard.

Il existe des alternatives comme écrire dans un fichier de log ou utiliser `syslog` sur les systèmes UNIX pour gérer les erreurs.

Techniquement, `stderr` est un flux tamponné, mais en pratique, souvent configuré pour être à vidage automatique (flush) après chaque sortie pour que les messages d'erreur apparaissent immédiatement.

## See Also (Voir aussi) :
- Documentation man de `fprintf`: http://man7.org/linux/man-pages/man3/fprintf.3.html
- GNU C Library - Standard Streams: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- Open Group Base Specifications - `stderr`: https://pubs.opengroup.org/onlinepubs/009695399/functions/stderr.html
