---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Télécharger une page web, c'est récupérer et stocker son contenu HTML. Les programmeurs le font pour analyser les données, extraire des informations ou encore automatiser des tâches.

## Comment faire :

Voici comment utiliser la bibliothèque C++ `cURL` pour télécharger une page web.

```C++
#include <iostream>
#include <curl/curl.h>

size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* s) {
    size_t newLength = size*nmemb;
    s->append((char*)contents, newLength);
    return newLength;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string s;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();

    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &s);

        res = curl_easy_perform(curl);

        if(res != CURLE_OK)
            std::cout << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        else
            std::cout << s << std::endl;

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```

## Plongée en profondeur :

Historiquement, télécharger une page web a toujours été nécessaire pour interagir avec elle sans navigateur. Il existe plusieurs manières de le faire en C++, et cURL est l'une des plus populaires. Elle offre une grande flexibilité et de nombreuses options pour personnaliser le téléchargement.

En alternative à cURL, vous pouvez utiliser des bibliothèques comme libSoup ou POCO HTTP Client. Chaque bibliothèque a ses propres avantages et inconvénients, il est donc important de choisir celle qui répond à vos besoins.

En ce qui concerne les détails d'implémentation, le code ci-dessus initialise cURL, définit l'URL à télécharger, associe une fonction de rappel pour enregistrer les données et enfin exécute l'opération. Quelques détails à noter : `CURLOPT_WRITEFUNCTION` définit la fonction de rappel, `CURLOPT_WRITEDATA` définit l'endroit où les données sont écrites, et curl_easy_perform() réalise l'opération de téléchargement.

## Voir aussi :

Pour approfondir le sujet, voici des liens vers des ressources connexes :

1. Documentation de cURL : [Lien](https://curl.haxx.se/libcurl/c/)
2. Tutoriel sur libSoup : [Lien](https://developer.gnome.org/libsoup/stable/)
3. Documentation de POCO HTTP Client : [Lien](https://pocoproject.org/docs/Poco.Net.HTTPClientSession.html)