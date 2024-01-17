---
title:                "Télécharger une page web"
html_title:           "C: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Télécharger une page web en programmation signifie récupérer le contenu d'une page web à partir de son URL. Les programmeurs le font souvent pour automatiser le processus de récupération de données à partir de sites web ou pour construire des outils de web scraping.

## Comment faire:

La bibliothèque standard C fournit la fonction `fopen()` pour ouvrir une page web et `fgets()` pour lire le contenu ligne par ligne. Voici un exemple de code pour télécharger une page web:

```C
#include <stdio.h>

int main () {
   FILE *file = fopen("https://www.example.com/", "r"); 
   char buffer[1024];
   if (file) {
        while (fgets(buffer, sizeof(buffer), file) != NULL) {  
            printf("%s", buffer); 
        }
        fclose(file); 
   }
   return 0;
}
```

La sortie de ce code sera le contenu de la page web imprimé dans la console.

## Plongée profonde:

À l'origine, les programmeurs utilisaient la fonction `gethostbyname()` pour résoudre un nom de domaine en adresse IP, puis établissaient une connexion TCP pour récupérer le contenu de la page. Aujourd'hui, la bibliothèque standard C fournit des fonctions plus faciles à utiliser telles que `fopen()` et `getc()`.

Alternativement, vous pouvez utiliser des bibliothèques tierces comme libcurl pour télécharger des pages web. Elles offrent plus de fonctionnalités, comme la prise en charge des protocoles HTTPS et FTP.

## À voir également:

Pour en savoir plus sur le téléchargement de pages web en C, consultez ces ressources:

- [Documentation de la bibliothèque standard C](https://en.cppreference.com/w/c)
- [Tutoriel sur l'utilisation de la bibliothèque libcurl](https://curl.haxx.se/libcurl/c)
- [Exemples de téléchargement de pages web en C](https://gist.github.com/ankushagarwal/6328278)