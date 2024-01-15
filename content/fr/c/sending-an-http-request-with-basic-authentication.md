---
title:                "Envoi d'une requête http avec une authentification de base"
html_title:           "C: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
Une des raisons les plus courantes pour envoyer une requête HTTP avec une authentification de base est pour accéder à une API protégée par mot de passe. Cela permet de restreindre l'accès à cette API à des utilisateurs authentifiés.

## Comment Faire
Pour envoyer une requête HTTP avec une authentification de base en utilisant la version actuelle du langage C, nous avons besoin de suivre ces étapes :

- Tout d'abord, nous devons importer la bibliothèque `curl` en utilisant la directive `#include <curl/curl.h>`.

- Ensuite, nous devons initialiser le gestionnaire CURL en utilisant la fonction `curl_easy_init()`. Nous stockons le résultat de cette fonction dans une variable de type `CURL*`.

- Ensuite, nous devons spécifier l'URL vers laquelle nous voulons envoyer notre requête en utilisant la fonction `curl_easy_setopt()` avec l'option `CURLOPT_URL` et l'URL en tant que argument.

- Maintenant, nous déclarons notre nom d'utilisateur et mot de passe à l'aide de la fonction `curl_easy_setopt()` avec les options `CURLOPT_USERNAME` et `CURLOPT_PASSWORD` respectivement.

- Enfin, nous sommes prêts à envoyer notre requête en utilisant la fonction `curl_easy_perform()`. Si tout se passe bien, cette fonction renvoie la valeur `CURLE_OK`.

Le code ressemblera à ceci :

```
#include <curl/curl.h>

int main(void) {
  CURL* curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/api");
  curl_easy_setopt(curl, CURLOPT_USERNAME, "utilisateur");
  curl_easy_setopt(curl, CURLOPT_PASSWORD, "motdepasse");
  CURLcode result = curl_easy_perform(curl);

  if(result != CURLE_OK) {
    printf("Erreur lors de l'envoi de la requête : %s\n",
           curl_easy_strerror(result));
  }

  curl_easy_cleanup(curl);
  return 0;
}
```

Si la requête est envoyée avec succès, la réponse sera stockée dans le gestionnaire CURL et nous pourrons l'utiliser pour afficher le résultat ou effectuer d'autres opérations.

## Plongée Profonde
La requête HTTP avec une authentification de base est envoyée avec l'en-tête `Authorization` qui contient les informations d'authentification encodées en base64. Voici comment l'en-tête peut être construit en utilisant la bibliothèque `curl` :

```
#include <curl/curl.h>

int construct_header_function(void *ptr, size_t size, size_t nmemb, void *stream) {
  strcpy(ptr, base64_encode("utilisateur:motdepasse"));
  return strlen(ptr);
}

int main(void) {
  CURL* curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/api");
  curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, &construct_header_function);
  CURLcode result = curl_easy_perform(curl);

  // Gestion des erreurs ici
}
```

Dans cet exemple, nous avons utilisé une fonction personnalisée, `construct_header_function`, pour construire l'en-tête. Dans cette fonction, nous appelons la fonction `base64_encode()` pour encoder le nom d'utilisateur et le mot de passe en base64 avant de les copier dans le pointeur `ptr` fourni par CURL.

## Voir aussi
- [Documentation sur la bibliothèque CURL](https://curl.haxx.se/libcurl/)
- [Base64 encode et decode en C](https://stackoverflow.com/questions/342409/how-do-i-base64-encode-decode-in-c)