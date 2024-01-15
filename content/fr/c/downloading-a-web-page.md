---
title:                "Téléchargement d'une page web"
html_title:           "C: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par le développement web ou si vous souhaitez créer un programme qui peut collecter des informations à partir d'un site web, alors vous devez savoir comment télécharger une page web en utilisant le langage de programmation C.

## Comment faire

Pour télécharger une page web en utilisant C, vous pouvez utiliser la bibliothèque standard du langage appelée "libcurl". Cette bibliothèque prend en charge les protocoles HTTP, HTTPS et FTP, et elle est facile à intégrer dans votre code. Voici un exemple simple de code pour télécharger une page web en utilisant libcurl :

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  FILE *file;
  CURLcode res;
  
  // Créer une instance CURL
  curl = curl_easy_init();
  
  // Ouvrir le fichier dans lequel écrire le contenu de la page web
  file = fopen("page.html", "w");
  
  // Définir l'URL à télécharger
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
  
  // Écrire le contenu de la page dans le fichier
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, file);
  
  // Exécuter la requête et vérifier le statut
  res = curl_easy_perform(curl);
  if(res != CURLE_OK) {
    printf("Erreur lors du téléchargement : %s\n",
          curl_easy_strerror(res));
  }
  
  // Nettoyer et fermer les ressources
  fclose(file);
  curl_easy_cleanup(curl);
  
  return 0;
}
```

Lorsque vous exécutez ce programme, il va créer un fichier "page.html" contenant le contenu de la page web téléchargée. Vous pouvez évidemment modifier le code pour écrire le contenu dans une variable plutôt que dans un fichier.

## Deep Dive

Si vous souhaitez en savoir plus sur la bibliothèque libcurl et ses fonctionnalités, vous pouvez consulter sa documentation officielle [ici](https://curl.se/libcurl/). Elle contient des informations détaillées sur toutes les options disponibles pour personnaliser votre requête de téléchargement.

De plus, il existe également d'autres bibliothèques et outils disponibles pour télécharger des pages web en utilisant C, tels que "libhtmlparser" et "httrack". N'hésitez pas à explorer et à trouver celui qui convient le mieux à vos besoins.

## Voir aussi

- [Documentation libcurl](https://curl.se/libcurl/)
- [Libhtmlparser](https://github.com/zserge/libhtmlparser)
- [Httrack](https://www.httrack.com/)