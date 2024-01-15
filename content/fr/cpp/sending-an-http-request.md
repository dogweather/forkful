---
title:                "Envoyer une requête http"
html_title:           "C++: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles vous pourriez vouloir envoyer une requête HTTP en utilisant C++. Cela peut être utile si vous souhaitez récupérer des données à partir d'une API web ou pour interagir avec un serveur distant.

## Comment faire

Tout d'abord, nous devons inclure la bibliothèque C++ appropriée pour envoyer des requêtes HTTP. Dans cet exemple, nous utiliserons `libcurl`, une bibliothèque open-source largement utilisée pour effectuer des requêtes HTTP. Voici comment inclure la bibliothèque dans notre programme :

```C++
#include <curl/curl.h>
```

Ensuite, nous devons initialiser un gestionnaire `CURL`. Cela peut être fait en utilisant la fonction `curl_easy_init()` qui renvoie un pointeur vers une structure de type `CURL`. Nous pouvons également utiliser `curl_global_init()` pour initialiser globalement `libcurl` au lieu de de le faire à chaque fois.

```C++
// Initialise globalement libcurl
curl_global_init(CURL_GLOBAL_DEFAULT);

// Initialise un gestionnaire CURL
CURL *curl = curl_easy_init();
```

Ensuite, nous devons définir l'URL que nous voulons appeler et préparer la requête. Dans cet exemple, nous allons appeler l'API Github pour récupérer des informations sur un utilisateur spécifique.

```C++
// URL de l'API Github pour récupérer les données de l'utilisateur
const char *url = "https://api.github.com/users/Utilisateur";

// Permet de recevoir la réponse de la requête
std::string response;

// Ajoute l'option de l'URL à la requête CURL
curl_easy_setopt(curl, CURLOPT_URL, url);

// Définit une fonction de rappel pour écrire la réponse
curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);

// Définit l'adresse de notre chaîne de réponse
curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
```

Enfin, nous pouvons exécuter la requête en utilisant la fonction `curl_easy_perform()` et récupérer la réponse dans notre chaîne `response`.

```C++
// Effectue la requête et récupère la réponse dans notre chaîne
CURLcode res = curl_easy_perform(curl);

// Vérifie si la requête a réussi
if (res != CURLE_OK) {
    // Gestionnaire d'erreur
    fprintf(stderr, "curl failed: %s\n", curl_easy_strerror(res));
} else {
    // La requête a réussi, imprimons la réponse
    std::cout << response << std::endl;
}

// Nettoie et libère les ressources
curl_easy_cleanup(curl);

// Libère les ressources libcurl initialisées
curl_global_cleanup();
```

## Deep Dive

Il est important de noter que l'utilisation de `libcurl` pour envoyer des requêtes HTTP en C++ n'est qu'une des nombreuses façons possibles de le faire. Il existe d'autres bibliothèques et outils, tels que `Boost.Asio` et `cpp-httplib`, qui peuvent également être utilisés pour effectuer des requêtes HTTP en utilisant C++.

Une chose à garder à l'esprit lors de l'envoi de requêtes HTTP est la sécurité. Assurez-vous de toujours utiliser HTTPS pour les connexions sécurisées et de ne pas inclure de données sensibles dans l'URL, car elles peuvent être visibles pour toute personne qui pourrait écouter le trafic réseau.

## Voir aussi

Voici quelques liens utiles pour en apprendre davantage sur l'envoi de requêtes HTTP en C++ :

- [Documentation officielle de libcurl](https://curl.haxx.se/libcurl/c/)
- [Boost.Asio pour les tâches réseau en C++](https://www.boost.org/doc/libs/1_76_0/doc/html/boost_asio.html)
- [cpp-httplib, une bibliothèque simple et légère pour effectuer des requêtes HTTP en C++](https://github.com/yhirose/cpp-httplib)