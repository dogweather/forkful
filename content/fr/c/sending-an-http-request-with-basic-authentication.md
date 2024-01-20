---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et Pourquoi?

Faire une requête HTTP avec une authentification de base, c'est simplement envoyer une requête HTTP qui inclut des informations d'authentification dans l'en-tête. Les développeurs le font pour accéder à des ressources sécurisées sur le web.

## Comment faire:

Voici un simple exemple en C avec libcurl:

```C 
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");

    struct curl_slist *headers = NULL;

    headers = curl_slist_append(headers, "Authorization: Basic base64(credentials)");

    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```

## Creuser un peu plus:

Historiquement, l'authentification de base HTTP a été développée comme un moyen simple de sécuriser les communications sur le web. Cependant, elle est devenue moins populaires car elle n'offre pas assez de sécurité par rapport aux alternatives comme l'authentification à jeton.

En utilisant `libcurl`, nous avons une facilité d'implémentation à notre disposition. N'oubliez pas que les informations d'authentification envoyées avec l'authentification de base HTTP sont codées en Base64, pas cryptées. Ce n'est pas un moyen sécurisé d'envoyer des informations sensibles sans une connexion sécurisée.

## Voir aussi:

[Pour comprendre les bases de l'utilisation de libcurl (En anglais)](https://curl.se/libcurl/c/)

[Pour plus d'informations sur les méthodes d'authentification HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication) 

[Documentation officielle Base64 (En anglais)](https://tools.ietf.org/html/rfc4648)