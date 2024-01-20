---
title:                "Envoyer une requête http"
html_title:           "Fish Shell: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'envoi d'une requête HTTP est une méthode permettant à votre programme d'interagir avec des ressources sur le web. Les programmeurs le font pour récupérer ou envoyer des données à des serveurs web.

## Comment faire :
Voici un exemple simple sur l'envoi d'une requête GET HTTP en utilisant la bibliothèque curl en C.

```C 
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode result;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    
    result = curl_easy_perform(curl);
    
    if(result != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(result));

    curl_easy_cleanup(curl);
  }
  
  curl_global_cleanup();
  
  return 0;
}
```
Dans cet exemple, nous avons envoyé une requête GET à "http://example.com".

## Poursuite
L'idée d'envoyer des requêtes HTTP a émergé avec le développement du World Wide Web. Il existe d'autres bibliothèques comme `libevent` et `libev` qui peuvent également être utilisées pour des requêtes HTTP. Le choix dépend de l'implémentation et des exigences spécifiques du logiciel.

## Voir Aussi
1. Documentation de libcurl : https://curl.haxx.se/libcurl/
2. HTTP : https://fr.wikipedia.org/wiki/Hypertext_Transfer_Protocol
3. Guide de démarrage libcurl : https://curl.haxx.se/docs/gettingstarted.html