---
title:                "Téléchargement d'une page web"
date:                  2024-01-20T17:43:35.393256-07:00
model:                 gpt-4-1106-preview
simple_title:         "Téléchargement d'une page web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Télécharger une page web, c'est récupérer son contenu via Internet pour le manipuler ou le stocker localement. Les programmeurs le font pour analyser des données, tester la disponibilité ou pour l'archivage.

## Comment faire :
Pour télécharger une page web en C, on utilise souvent `libcurl`. Voici un exemple simple :

```C
#include <stdio.h>
#include <curl/curl.h>

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
    size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://exemple.com";
    char outfilename[FILENAME_MAX] = "./exemple.html";
    
    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```

Résultat : La page web est sauvegardée en tant que `exemple.html`.

## Plongée en profondeur
`Libcurl`, créé en 1998, est un client de requêtes URLs performant. Des alternatives incluraient `libwww` ou des appels système à `wget`. L’implémentation varie selon la complexité, `libcurl` étant idéal pour les opérations simples à complexes grâce à sa flexibilité et sa portabilité.

## Voir également
- Documentation `libcurl` : https://curl.se/libcurl/
- Tutoriel `libcurl` pour débutants : https://curl.se/libcurl/c/libcurl-tutorial.html
- Comparaison des bibliothèques client HTTP : https://en.wikipedia.org/wiki/Comparison_of_HTTP_library

Souvenez-vous que télécharger des pages sans permission peut enfreindre des conditions d'utilisation. Utilisez ces outils de façon responsable.