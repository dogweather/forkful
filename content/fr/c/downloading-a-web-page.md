---
title:                "C: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous êtes-vous déjà demandé comment une page web est téléchargée et affichée sur votre écran? Cela peut sembler simple, mais en réalité, il y a beaucoup de travail qui se passe en arrière-plan pour que cela se produise. Dans cet article, nous allons apprendre à utiliser le langage de programmation C pour télécharger des pages web.

## Comment Faire
Pour télécharger une page web en utilisant C, nous allons utiliser la bibliothèque standard "stdio.h" pour les entrées et sorties, ainsi que la bibliothèque "curl.h" pour effectuer les requêtes de téléchargement. Voici un exemple de code pour télécharger une page web:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;
  FILE *fp;
  char *url = "https://www.exemple.com";
  char outfilename[FILENAME_MAX] = "page.html";
  
  curl = curl_easy_init();
  
  if(curl) {
    fp = fopen(outfilename,"wb");
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    res = curl_easy_perform(curl);
    
    curl_easy_cleanup(curl);
    fclose(fp);
  }
  
  return 0;
}
```

Dans cet exemple, nous utilisons la fonction "curl_easy_init" pour initialiser une session CURL. Ensuite, nous utilisons la fonction "fopen" pour créer un fichier dans lequel la page web téléchargée sera enregistrée. Ensuite, nous définissons l'URL cible en utilisant la fonction "curl_easy_setopt" et enfin, nous utilisons "curl_easy_perform" pour effectuer la requête de téléchargement.

## Plongée Profonde
Maintenant, regardons de plus près certains des paramètres que nous avons utilisés dans cet exemple de code. Tout d'abord, le paramètre "CURLOPT_URL" définit l'URL de la page web que nous voulons télécharger. Ensuite, le paramètre "CURLOPT_WRITEFUNCTION" spécifie quelle fonction sera utilisée pour écrire les données téléchargées dans le fichier. Ici, nous avons utilisé NULL car nous n'avons pas besoin d'une fonction spécifique, mais vous pouvez en utiliser une si nécessaire. Le paramètre "CURLOPT_WRITEDATA" définit ensuite le fichier dans lequel les données seront écrites.

## Voir Aussi
Pour en savoir plus sur la bibliothèque "curl.h" et ses fonctions, vous pouvez consulter leur documentation ici: https://curl.haxx.se/libcurl/. Vous pouvez également trouver de nombreux exemples de code en ligne pour vous aider à télécharger des pages web en utilisant C. Alors, amusez-vous à explorer cette fonctionnalité intéressante du langage C!