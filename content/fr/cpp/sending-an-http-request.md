---
title:                "C++: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi 

Les requêtes HTTP sont des éléments fondamentaux de la programmation web moderne. Elles permettent de communiquer avec des serveurs distants pour récupérer des données ou effectuer des actions. Apprendre à envoyer des requêtes HTTP en C++ est donc essentiel pour tout développeur web.

## Comment faire 

Il existe différentes façons d'envoyer une requête HTTP en C++. La méthode la plus courante est d'utiliser une librairie de gestion de requêtes, telle que cURL ou Boost.Asio. Voici un exemple de code pour envoyer une requête GET en utilisant cURL :

```C++
CURL *curl;
CURLcode res;

curl = curl_easy_init();
if(curl) {
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
  res = curl_easy_perform(curl);
  curl_easy_cleanup(curl);
}
```

Ceci enverra une requête HTTP GET à l'adresse "https://www.example.com/" et stockera le résultat dans la variable `res`. Il est possible de personnaliser la requête en ajoutant des headers ou en envoyant des données avec la requête.

## Plongée en profondeur 

Nous venons de voir un exemple simple d'envoi de requête HTTP en utilisant cURL. Cependant, il est important de comprendre les différents éléments qui composent une requête HTTP. Tout d'abord, une requête se compose d'une ligne de requête, suivie d'headers et éventuellement de corps de requête. La ligne de requête contient la méthode de la requête (GET, POST, PUT, etc.), l'URI et la version du protocole HTTP utilisée.

Les headers quant à eux contiennent des informations supplémentaires sur la requête, telles que le type de contenu envoyé ou accepté, des cookies, etc. Enfin, le corps de requête contient les données envoyées avec la requête, généralement utilisé pour les requêtes POST ou PUT.

## Voir aussi 

- [Documentation sur cURL](https://curl.haxx.se/libcurl/c/)
- [Documentation sur Boost.Asio](https://www.boost.org/doc/libs/1_76_0/doc/html/boost_asio.html)
- [Introduction aux requêtes HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Overview)