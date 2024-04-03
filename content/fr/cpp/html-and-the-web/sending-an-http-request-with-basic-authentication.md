---
date: 2024-01-20 18:01:19.271765-07:00
description: "Envoyer une requ\xEAte HTTP avec authentification basique, c'est transmettre\
  \ vos identifiants (normalement un nom d'utilisateur et mot de passe) cod\xE9s en\u2026"
lastmod: '2024-03-13T22:44:58.161168-06:00'
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP avec authentification basique, c'est transmettre\
  \ vos identifiants (normalement un nom d'utilisateur et mot de passe) cod\xE9s en\
  \ base64 dans l'en-t\xEAte de la requ\xEAte pour acc\xE9der \xE0 des ressources\
  \ prot\xE9g\xE9es."
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
weight: 45
---

## Quoi & Pourquoi ?
Envoyer une requête HTTP avec authentification basique, c'est transmettre vos identifiants (normalement un nom d'utilisateur et mot de passe) codés en base64 dans l'en-tête de la requête pour accéder à des ressources protégées. Les programmeurs l'utilisent pour interagir avec des API web qui exigent une forme simple d'authentification.

## Comment faire :
Pour envoyer une requête HTTP avec authentification basique en C++, on peut utiliser la bibliothèque cURL. Voici un exemple minimaliste :

```C++
#include <curl/curl.h>
#include <iostream>
#include <string>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        const std::string userPwd = "user:password"; // Remplacez par vos données
        curl_easy_setopt(curl, CURLOPT_URL, "http://monapi.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, userPwd.c_str());

        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            std::cerr << "Erreur curl: " << curl_easy_strerror(res) << std::endl;
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Sortie attendue (elle sera différente selon le résultat de l'API) :

```
... données récupérées depuis l'API ...
```

## Plongée profonde
L'authentification basique HTTP est une méthode standardisée depuis longtemps (RFC 7617), simple mais pas la plus sûre. Les identifiants sont encodés en Base64, mais ce n'est pas un chiffrement sécurisé. 

Alternatives : OAuth, tokens d'API, ou authentification par certificat client sont plus sécurisés. Utiliser HTTPS est essentiel pour protéger les données.

Détails techniques : cURL gère bien l'authentification basique sans ajouter beaucoup de code. `curl_easy_setopt()` est utilisée pour configurer les différentes options, notamment URL cible, méthode d'authentification et identifiants. `curl_easy_perform()` exécute la requête. La gestion des erreurs est simplifiée grâce à `curl_easy_strerror()` qui traduit les codes d'erreur cURL en messages lisibles.

## Voir aussi
- Documentation de cURL : https://curl.haxx.se/libcurl/c/
- RFC 7617, "The 'Basic' HTTP Authentication Scheme" : https://tools.ietf.org/html/rfc7617
- Authentification HTTP sur MDN Web Docs : https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
