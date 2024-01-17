---
title:                "Envoi d'une requête http avec authentification de base"
html_title:           "C++: Envoi d'une requête http avec authentification de base"
simple_title:         "Envoi d'une requête http avec authentification de base"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire ?

L'envoi d'une requête HTTP avec une authentication de base est une façon standard pour sécuriser les communications entre un client et un serveur web. Les programmeurs le font pour s'assurer que seules les personnes autorisées puissent accéder à certaines ressources.

# Comment faire :

```C++
#include <iostream>
#include <curl/curl.h> // Pour utiliser la librairie cURL
 
int main(){
    // Initialiser ses identifiants de connexion
    std::string username = "mon_nom_d'utilisateur";
    std::string password = "mon_mot_de_passe";
    
    // Initialiser la requête HTTP
    CURL *curl;
    curl = curl_easy_init();
    if(curl) {
        // Définir l'URL de la ressource cible
        curl_easy_setopt(curl, CURLOPT_URL, "https://mon-site-web.com/chemin_vers_ressource");
        // Ajouter l'authentication de base à la requête
        curl_easy_setopt(curl, CURLOPT_USERPWD, (username + ":" + password).c_str());
        
        // Envoyer la requête
        CURLcode res = curl_easy_perform(curl);
        
        // Vérifier si la requête a réussi
        if(res == CURLE_OK) {
            std::cout << "Requête envoyée avec succès !";
        }
        else {
            std::cerr << "Erreur lors de l'envoi de la requête : " << curl_easy_strerror(res) << std::endl;
        }
        
        // Nettoyer
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

# Plongée en profondeur :

L'authentication de base est un mécanisme d'authentification simple où les informations d'identification (nom d'utilisateur et mot de passe) sont encodées en base64 et incluses dans l'en-tête Authorization de la requête HTTP. Cette méthode est considérée comme étant moins sécurisée que d'autres méthodes telles que l'OAuth, car les informations d'identification peuvent être facilement décodées. Cependant, elle reste largement utilisée dans certaines applications où la sécurité n'est pas une priorité absolue.

# Voir aussi :

- [Documentation cURL](https://curl.se/docs/)
- [L'authentication HTTP expliquée](https://www.veracode.com/security/authentication/http-authentication)
- [Guide de l'OAuth pour les débutants](https://blog.restcase.com/restful-api-oauth2-how-to-get-started/)