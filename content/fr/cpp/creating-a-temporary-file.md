---
title:                "Création d'un fichier temporaire"
html_title:           "C++: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en C ++

La création d'un fichier temporaire en C ++ peut être utile pour stocker des données temporaires telles que des résultats de calculs ou des fichiers téléchargés, sans avoir à les stocker en permanence sur le système.

## Comment faire

Utiliser la fonction `tmpfile()` pour créer un fichier temporaire et écrire les données souhaitées dessus. Voici un exemple de code :

```C++
#include <stdio.h>

int main() {
  FILE *fptr;
  int num = 10;
  fptr = tmpfile();
  fprintf(fptr, "%d", num);
  fclose(fptr);
  return 0;
}
```

La sortie de ce code créera un fichier temporaire contenant le nombre 10. Notez que le fichier sera automatiquement supprimé à la fermeture du programme.

## Plongée en profondeur

La fonction `tmpfile()` crée un fichier binaire en lecture/écriture et le stocke dans un dossier temporaire du système. Le fichier est généralement nommé de manière aléatoire pour éviter les conflits avec d'autres fichiers temporaires. Il est également important de vérifier si le fichier a bien été créé avant d'écrire dessus, en utilisant la fonction `fptr != NULL`.

## Voir aussi

- [Documentation officielle de `tmpfile()` en C++](https://en.cppreference.com/w/cpp/io/c/tmpfile)
- [Tutorial sur l'utilisation des fichiers temporaires en C](https://www.guru99.com/c-file-input-output.html)