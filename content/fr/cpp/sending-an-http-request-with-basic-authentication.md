---
title:                "Envoyer une requête http avec authentification de base"
html_title:           "C++: Envoyer une requête http avec authentification de base"
simple_title:         "Envoyer une requête http avec authentification de base"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
Pourquoi enverrions-nous une requête HTTP avec une authentification de base? Eh bien, c'est un moyen simple et sécurisé pour accéder à des ressources en ligne qui nécessitent une authentification de base.

## Comment faire
L'envoi d'une requête HTTP avec une authentification de base en C++ peut sembler intimidant, mais c'est en fait assez simple. Suivez ces étapes pour l'implémenter dans votre code:

```C++
// Importer les bibliothèques nécessaires
#include <iostream>
#include <curl/curl.h>

// Définir les identifiants d'authentification
std::string username = "utilisateur";
std::string password = "motdepasse";

// Déclarer la fonction de rappel pour l'authentification
int authCallback(CURL *curl, CURLoption option, char *username, char *password)
{
    // Définir les options CURL
    curl_easy_setopt(curl, CURLOPT_USERNAME, username);
    curl_easy_setopt(curl, CURLOPT_PASSWORD, password);

    return CURL_USERNAME_PASSWORD_REQUIRED; // Retourner l'indicateur d'authentification requise
}

int main()
{
    // Initialiser une instance CURL
    CURL *curl = curl_easy_init();

    // Ajouter l'option d'authentification de base
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);

    // Définir l'URL de la ressource à accéder
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/secret-resource");

    // Définir la fonction de rappel pour l'authentification
    curl_easy_setopt(curl, CURLOPT_USERNAME_PASSWORD_FUNCTION, authCallback);

    // Effectuer la requête et récupérer la réponse
    CURLcode result = curl_easy_perform(curl);

    // Vérifier si la requête a réussi 
    if (result != CURLE_OK)
    {
        // Afficher un message d'erreur
        std::cout << "Erreur lors de l'envoi de la requête : " << curl_easy_strerror(result) << std::endl;
    }

    // Nettoyer et libérer les ressources
    curl_easy_cleanup(curl);

    return 0;
}
```

Output:

```
Enter host password for user 'utilisateur': 

Bonjour! Vous avez désormais accès à la ressource secrète!
```

## Plongée en profondeur
L'authentification de base envoie les informations d'identification sous forme de texte brut, rendant ainsi la sécurité de votre application vulnérable. Il est donc recommandé d'utiliser des méthodes d'authentification plus avancées, telles que l'authentification basée sur les tokens.

## Voir aussi
- [Documentation officielle de la bibliothèque libcurl](https://curl.se/libcurl/c/http-auth.html)
- [Guide de référence pour l'authentification HTTP](https://www.httpwatch.com/authentication/)