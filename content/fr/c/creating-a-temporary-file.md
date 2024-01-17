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

# Quoi et Pourquoi?

Lorsque les programmeurs travaillent avec des fichiers dans leurs programmes C, ils peuvent parfois avoir besoin de créer un fichier temporaire. Un fichier temporaire est un fichier qui ne sera pas sauvegardé une fois le programme terminé. Les programmeurs utilisent généralement des fichiers temporaires pour stocker des données temporaires ou pour tester des fonctionnalités avant de les implémenter définitivement.

# Comment faire:

Voici un exemple de code pour créer un fichier temporaire en utilisant la fonction ```tmpfile()``` en C: 

```
#include <stdio.h>

int main(void) {
    FILE *req_file = NULL;
    req_file = tmpfile();
    
    if (req_file != NULL) {
        printf("Fichier temporaire créé avec succès.\n");
    }
    
    return 0;
}
```

La sortie de ce code sera ```Fichier temporaire créé avec succès.```.

# Plongée en profondeur:

La création de fichiers temporaires est une pratique courante en programmation C, remontant aux premières versions du langage. Cependant, avec l'évolution de la technologie, il existe aujourd'hui des alternatives telles que l'utilisation de mémoires tampons en lieu et place des fichiers temporaires. Cette approche peut être plus efficace dans certaines situations car elle évite la gestion de fichiers supplémentaires.

La fonction ```tmpfile()``` est implémentée dans la bibliothèque standard C. Elle crée un fichier temporaire dans le répertoire par défaut pour les fichiers temporaires de l'utilisateur. Si vous souhaitez modifier cet emplacement, il existe d'autres fonctions telles que ```tmpnam()``` et ```tmpnam_r()``` qui vous permettent de spécifier un chemin personnalisé pour votre fichier temporaire.

# Voir aussi:

Pour plus d'informations sur la création de fichiers temporaires en C, vous pouvez consulter la documentation officielle de la fonction ```tmpfile()``` sur le site officiel de GNU. Vous pouvez également explorer d'autres alternatives telles que l'utilisation de mémoires tampons ou la création de fichiers temporaires avec des noms personnalisés.