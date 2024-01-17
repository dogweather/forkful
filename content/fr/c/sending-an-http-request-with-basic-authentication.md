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

##Quoi & Pourquoi?
L'envoi d'une requête HTTP avec une authentification de base est une méthode utilisée par les programmeurs pour accéder à des ressources protégées sur le Web. Cela permet aux utilisateurs de s'authentifier en utilisant un nom d'utilisateur et un mot de passe avant d'accéder aux données. Cela garantit également la sécurité des informations sensibles.

##Comment faire:
Voici un exemple simple de code en C montrant comment envoyer une requête HTTP avec une authentication de base:

```C
#include <stdio.h>
#include <curl/curl.h>
 
// URL de la ressource protégée
#define URL "https://example.com/protected_resource"
// Nom d'utilisateur et mot de passe pour l'authentification de base
#define USERNAME "utilisateur"
#define PASSWORD "motdepasse"
 
int main(void)
{
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    // Définition de l'URL
    curl_easy_setopt(curl, CURLOPT_URL, URL);
    // Activation de l'authentification de base
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    // Définition du nom d'utilisateur et du mot de passe
    curl_easy_setopt(curl, CURLOPT_USERNAME, USERNAME);
    curl_easy_setopt(curl, CURLOPT_PASSWORD, PASSWORD);
    // Envoi de la requête et affichage de la réponse
    res = curl_easy_perform(curl);
    printf("Réponse: %s\n", curl_easy_strerror(res));
    // Nettoyage
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
Exemple de sortie:
```
Réponse: OK
```

##Profonde plongée:
L'authentification de base est une méthode simple et largement utilisée pour sécuriser les ressources sur le Web. Elle a été introduite dans la spécification HTTP en 1999 et utilise un encodage base64 pour envoyer le nom d'utilisateur et le mot de passe dans l'en-tête de la requête. Bien qu'elle soit facile à implémenter, elle a l'inconvénient de ne pas être sécurisée pour les échanges sur des réseaux non cryptés. D'autres méthodes d'authentification plus sécurisées, telles que l'authentification OAuth, peuvent être utilisées en fonction des besoins spécifiques du projet.

##À voir également:
- [Documentation officielle de libcurl sur l'authentification de base en C](https://curl.haxx.se/libcurl/c/CURLOPT_HTTPAUTH.html)
- [Spécification HTTP sur l'authentification de base](https://tools.ietf.org/html/rfc2617)