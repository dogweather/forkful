---
title:                "Envoi d'une requête http"
html_title:           "C++: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est?

Envoyer une requête HTTP est une action courante pour les programmeurs. Cela implique de communiquer avec un serveur web en utilisant un protocole spécifique appelé Hypertext Transfer Protocol (HTTP). Les développeurs le font souvent pour récupérer des données à partir d'une API ou pour envoyer des informations à un site web.

## Comment faire:

```C++
#include <iostream>
#include <curl/curl.h>
using namespace std;

int main() {
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://exemple.com");
        res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            cerr << "Erreur lors de l'envoi de la requête : " << curl_easy_strerror(res) << '\n';
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

La sortie de ce code sera un code HTML brut provenant du site web.

## Profondeur de plongée:

L'envoi de requêtes HTTP remonte aux débuts d'Internet et est un élément essentiel de l'interaction entre les client et les serveurs web. Les alternatives à l'envoi de requêtes HTTP incluent l'utilisation de protocoles différents tels que FTP ou SMTP, ainsi que l'utilisation de bibliothèques telles que cURL. Dans l'exemple ci-dessus, nous utilisons la bibliothèque cURL pour simplifier l'envoi de la requête.

## Voir aussi:

- [Documentation cURL](https://curl.haxx.se/libcurl/)
- [Introduction à HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Overview)