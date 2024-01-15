---
title:                "Envoyer une requête http"
html_title:           "C: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Envoyer une requête HTTP est une tâche essentielle dans le développement d'applications web. Cela permet d'échanger des informations entre un client et un serveur, ce qui est indispensable pour toute communication sur internet.

## Comment Faire

Pour envoyer une requête HTTP en utilisant le langage C, il y a quelques étapes à suivre :

```C
#include <stdlib.h>
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
   CURL *curl;
   CURLcode res;
   
   // Création de l'objet curl
   curl = curl_easy_init();

   if(curl) {
      // Définition de l'URL de destination
      curl_easy_setopt(curl, CURLOPT_URL, "https://www.cours.fr/api");

      // Définition de l'action à effectuer (GET, POST, etc.)
      curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "GET");

      // Exécution de la requête
      res = curl_easy_perform(curl);

      // Vérification des erreurs
      if(res != CURLE_OK)
         fprintf(stderr, "Erreur lors de l'envoi de la requête : %s\n",
                 curl_easy_strerror(res));

      // Nettoyage de l'objet curl
      curl_easy_cleanup(curl);
   }
   return 0;
}
```

Pour cet exemple, nous utilisons la librairie `libcurl` qui fournit des fonctions pour faciliter l'envoi de requêtes HTTP. Tout d'abord, nous créons un objet `curl` et définissons l'URL de destination et l'action à effectuer. Ensuite, nous exécutons la requête en utilisant la fonction `curl_easy_perform` et vérifions s'il y a eu des erreurs. Enfin, nous nettoyons l'objet `curl` pour libérer la mémoire.

Il est également possible de définir des en-têtes et des corps de requête en utilisant les options `CURLOPT_HTTPHEADER` et `CURLOPT_POSTFIELDS`. Pour plus d'exemples et d'informations, consultez la documentation de `libcurl`.

## Plongée en Profondeur

L'envoi d'une requête HTTP avec C peut sembler complexe, mais il s'agit en fait d'une tâche relativement simple grâce à `libcurl`. Cette librairie offre également de nombreuses fonctionnalités avancées pour gérer les cookies, les certificats SSL, et bien d'autres.

Il est important de bien comprendre le fonctionnement des requêtes HTTP et des réponses pour éviter les erreurs et optimiser les performances de votre application. N'hésitez pas à vous plonger dans la documentation si vous avez besoin de plus d'informations.

## Voir Aussi

- [Documentation `libcurl`](https://curl.haxx.se/libcurl/c/)
- [Protocol HTTP sur MDN](https://developer.mozilla.org/fr/docs/Apprendre/JavaScript/Client-server_web_APIs/Fetching_data_with_XMLHttpRequest)
- [Exemples de requêtes HTTP](https://httpbin.org/)