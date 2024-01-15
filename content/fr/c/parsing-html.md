---
title:                "Analyser du html"
html_title:           "C: Analyser du html"
simple_title:         "Analyser du html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous vous intéressez à la programmation en C, vous avez probablement entendu parler de l'HTML (Hypertext Markup Language). Ce langage de balisage est utilisé pour créer des pages web, et il est essentiel de pouvoir le convertir en code informatique pour interagir avec ces pages. C'est là qu'intervient le parsing HTML en C.

## Comment faire
Voici un exemple simple de parsing HTML en C pour récupérer le titre d'une page web :

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  // Ouvrir le fichier HTML
  FILE *fptr;
  fptr = fopen("page_web.html", "r");
  char buffer[1000];

  // Lire le contenu HTML dans une chaîne de caractères
  while (fgets(buffer, 1000, fptr)) {
    // Utiliser la fonction strstr() pour rechercher le titre
    if (strstr(buffer, "<title>") != NULL) {
      // Extraire le titre entre les balises
      char *start = strchr(buffer, '>') + 1;
      char *end = strrchr(buffer, '<');
      *end = '\0';
      char *title = malloc(strlen(start) + 1);
      strcpy(title, start);

      // Afficher le titre
      printf("Titre de la page : %s\n", title);
      // Vous pouvez également stocker le titre dans une variable pour une utilisation ultérieure
      break;
    }
  }
  fclose(fptr);
  return 0;
}
```

### Output:
```
Titre de la page : Cours de programmation en C
```

Cet exemple utilise la fonction `strstr()` pour rechercher la première occurrence de la balise `<title>`. Ensuite, il utilise les fonctions `strchr()` et `strrchr()` pour extraire le contenu entre les balises et l'afficher. Bien entendu, il existe de nombreuses autres façons de parser le HTML en C, mais cet exemple vous donne une idée de base du processus.

## Plongée en profondeur
Il est important de comprendre que le parsing HTML en C peut être un peu plus complexe que l'exemple ci-dessus. Pour gérer tous les scénarios possibles, il est souvent nécessaire d'utiliser des bibliothèques spécialisées telles que `libxml2` ou `libhtmlparser`.

De plus, il est important de noter que le parsing HTML en C peut être plus lent que dans d'autres langages, en raison de son manque de fonctionnalités de manipulation de chaînes de caractères. Il est donc important d'optimiser votre code autant que possible pour améliorer les performances.

## Voir aussi
- [Documentation sur la bibliothèque libxml2](http://www.xmlsoft.org/html/)
- [Documentation sur la bibliothèque libhtmlparser](https://htmlparser.sourceforge.io/)
- [Tutoriel sur le parsing HTML en C](https://www.geeksforgeeks.org/parsing-html-using-c/)