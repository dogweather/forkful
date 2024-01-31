---
title:                "Téléchargement d'une page web"
date:                  2024-01-20T17:43:34.454595-07:00
model:                 gpt-4-1106-preview
simple_title:         "Téléchargement d'une page web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Télécharger une page web, c'est récupérer son contenu via internet. Les programmeurs le font pour automatiser la collecte d'informations, tester la disponibilité d'un site ou analyser des données web.

## How to:
En C++, on peut utiliser la bibliothèque `libcurl` pour télécharger une page web. Installez `libcurl` si ce n'est pas déjà fait. Voici un exemple simple :

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);

        if(res == CURLE_OK)
            std::cout << readBuffer << std::endl;
        else
            std::cerr << "Erreur: " << curl_easy_strerror(res) << std::endl;
    }
    return 0;
}
```

Résultat (échantillon) :

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive
Le protocole HTTP sert à télécharger les pages web depuis 1991. `libcurl` est robuste, supporte FTP/FTPS, HTTP/HTTPS, et d'autres protocoles, ce qui en fait l'outil idéal. Comme alternative, vous pouvez utiliser `Poco::Net` ou d'autres bibliothèques HTTP pour C++. Le détail d'implémentation avec `libcurl` comprend la gestion des callbacks pour le transfert de données et la configuration des options avec `curl_easy_setopt`.

## See Also

- Documentation `libcurl`: https://curl.se/libcurl/
- `Poco::Net` Library: https://pocoproject.org/docs/Poco.Net.HTTPClientSession.html
- Le protocole HTTP: https://developer.mozilla.org/fr/docs/Web/HTTP
