---
title:                "Écriture d'un fichier texte."
html_title:           "C: Écriture d'un fichier texte."
simple_title:         "Écriture d'un fichier texte."
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est l'écriture d'un fichier texte et pourquoi les programmeurs le font-ils?

L'écriture d'un fichier texte est le processus de création d'un fichier contenant du texte brut. Cela peut inclure du texte dans différents formats tels que ASCII, UTF-8 ou ANSI, mais il s'agit toujours de caractères lisibles par l'homme plutôt que de données binaires. Les programmeurs écrivent souvent des fichiers texte pour stocker des données telles que des configurations ou des données à utiliser dans leur programme.

Comment faire?

Voici un exemple de code en C pour créer un fichier texte et y écrire du contenu:

```C
#include <stdio.h>

int main() {
  // Ouvre le fichier en mode écriture
  FILE *fptr = fopen("monfichier.txt", "w");

  // Vérifie s'il y a une erreur à l'ouverture du fichier
  if (fptr == NULL) {
    printf("Erreur lors de l'ouverture du fichier!");
    return 1;
  }

  // Écriture de la chaîne de caractères dans le fichier
  fprintf(fptr, "Cet article est écrit en C.");

  // Ferme le fichier
  fclose(fptr);
  
  return 0;
}
```

En exécutant ce code, un fichier "monfichier.txt" sera créé contenant la phrase "Cet article est écrit en C." Vous pouvez également utiliser des fonctions comme `fputs` ou `fwrite` pour écrire du contenu dans un fichier texte.

Plongée en profondeur

L'écriture de fichiers texte a été un élément fondateur de la programmation depuis les premiers langages de programmation. Avant l'utilisation répandue des bases de données, les fichiers texte étaient souvent la seule façon de stocker et de récupérer des données dans un programme. Maintenant, il existe des alternatives plus efficaces pour stocker des données, comme les bases de données, mais l'écriture de fichiers texte est toujours utile pour stocker des données simples ou pour des opérations de traitement de texte brut.

Voir aussi

Si vous souhaitez en savoir plus sur l'écriture de fichiers texte en C, vous pouvez consulter la documentation officielle de la fonction d'écriture de fichiers `fprintf`, ainsi que des tutoriels en ligne pour des exemples de code plus avancés. Vous pouvez également explorer d'autres alternatives telles que l'utilisation de fichiers CSV pour stocker des données structurées.