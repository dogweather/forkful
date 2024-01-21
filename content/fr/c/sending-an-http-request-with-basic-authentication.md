---
title:                "Envoi d'une requête HTTP avec authentification de base"
date:                  2024-01-20T18:00:59.079850-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP avec authentification de base"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Envoyer une requête HTTP avec une authentification de base, c'est communiquer avec un serveur tout en fournissant un nom d'utilisateur et un mot de passe encodés. Les programmeurs font cela pour accéder à des ressources sécurisées sur le web.

## How to:

Pour envoyer une requête HTTP avec authentification basique en C, on utilise souvent libcurl. Voici un exemple concis :

```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");
        
        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Sortie attendue: `(La sortie dépendra de la ressource spécifique et du serveur consultés)`

## Deep Dive

Historiquement, l'authentification de base en HTTP a été une des premières méthodes pour sécuriser l'accès aux ressources web. Cependant, elle n'offre qu'un niveau de sécurité minimal et peut être vulnérable à des attaques de type 'man-in-the-middle' si non utilisée avec HTTPS. Des alternatives plus sûres comme OAuth ou JWT (JSON Web Tokens) gagnent en popularité. Dans l'implémentation, le plus important est l'encodage Base64 du nom d'utilisateur et du mot de passe qui sont passés dans l'entête `Authorization` de la requête HTTP. La librairie libcurl s'occupe de cet encodage automatiquement.

## See Also

- CURL library: https://curl.se/libcurl/
- HTTP authentication basics: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Base64 encoding: https://en.wikipedia.org/wiki/Base64
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Security considerations (HTTPS & alternatives): https://www.owasp.org