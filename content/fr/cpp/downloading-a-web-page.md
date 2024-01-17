---
title:                "Le téléchargement d'une page Web"
html_title:           "C++: Le téléchargement d'une page Web"
simple_title:         "Le téléchargement d'une page Web"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Télécharger une page web signifie simplement récupérer son contenu à partir d'Internet. Les programmeurs le font souvent pour extraire des informations utiles à partir d'une page web et les utiliser dans leurs programmes.

## Comment faire:

```C++
#include <iostream>
#include <curl/curl.h> // Bibliothèque pour le téléchargement

using namespace std;

int main()
{
    CURL *curl; 
    CURLcode res; 
    string url = "https://www.example.com"; // URL à télécharger

    curl = curl_easy_init(); // Initialiser l'objet curl
    if(curl) 
    {
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str()); // Définir l'URL à télécharger
        res = curl_easy_perform(curl); // Exécuter la requête
        curl_easy_cleanup(curl); // Nettoyer l'objet curl
    }
    
    return 0;
}
```

**Résultat:**

<img src="https://user-images.githubusercontent.com/73485858/119124235-07ec5e00-ba31-11eb-89cc-8651029d1535.png" width="450">

## Plongée en profondeur:

Les programmeurs ont souvent besoin de télécharger des pages web pour extraire des données ou automatiser certaines tâches, telles que la mise à jour de leur base de données avec des informations récentes. Le téléchargement de pages web peut également être utilisé pour créer des "bots" ou des outils de surveillance basés sur le web. Il existe plusieurs bibliothèques disponibles en plus de cURL pour effectuer des téléchargements en C++, telles que libcurl et boost::asio.

## Voir aussi:

- [Curl documentation](https://curl.se/docs/)
- [libcurl documentation](https://curl.se/libcurl/)
- [boost::asio documentation](https://www.boost.org/doc/libs/1_76_0/doc/html/boost_asio.html)