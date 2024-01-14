---
title:                "C: Envoi d'une requête http avec une authentification de base"
simple_title:         "Envoi d'une requête http avec une authentification de base"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Pourquoi

L'envoi d'une requête HTTP avec une authentification de base est une méthode couramment utilisée pour accéder à des ressources en ligne protégées par un mot de passe. Cela peut être utile pour sécuriser les informations confidentielles telles que les données de connexion ou les transactions bancaires.

# Comment faire

Pour commencer, il est important d'avoir des connaissances de base en langage C et en protocole HTTP. Pour envoyer une requête avec une authentification de base, vous aurez besoin de la bibliothèque "curl" qui peut être installée facilement à l'aide de gestionnaires de paquets comme apt ou yum.

Voici un exemple de code pour envoyer une requête GET avec une authentification de base en utilisant curl :

```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void) {

    // Initialise la structure de requête curl
    CURL *curl = curl_easy_init();
    
    // Définit l'URL cible et les informations d'authentification
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
    
    // Exécute la requête et stocke la réponse dans une variable
    CURLcode res = curl_easy_perform(curl);
    
    // Affiche le code de réponse HTTP et la réponse
    long response_code;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
    if (res != CURLE_OK)
        fprintf(stderr, "curl_easy_perform() failed : %s\n", curl_easy_strerror(res));
    else
        printf("Réponse : %ld\n", response_code);
    
    // Nettoie et libère la mémoire
    curl_easy_cleanup(curl);
    
    return 0;
}
```

Lors de l'exécution de ce code, vous devriez recevoir une réponse avec le code HTTP 200, indiquant que la requête a réussi.

# Plongée en profondeur

Maintenant que nous avons vu un exemple de code simple, il est important de comprendre comment cela fonctionne réellement. Lorsque vous définit une URL cible avec `curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com")`, curl crée un objet de requête. Avec `curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC)`, nous spécifions que cette requête nécessite une authentification de base. Ensuite, avec les options `CURLOPT_USERNAME` et `CURLOPT_PASSWORD`, nous fournissons les informations d'identification nécessaires pour nous connecter.

Lorsque la requête est exécutée avec `curl_easy_perform(curl)`, curl envoie la requête à l'URL cible et attend une réponse. Une fois la réponse reçue, elle est stockée dans une variable et nous pouvons la récupérer en utilisant `curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code)`.

N'oubliez pas de nettoyer et de libérer la mémoire avec `curl_easy_cleanup(curl)` après avoir terminé d'utiliser la bibliothèque curl.

# Voir aussi

- [Documentation de la bibliothèque curl](https://curl.se/libcurl/)
- [Guide de référence HTTP](https://tools.ietf.org/html/rfc7231)