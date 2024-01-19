---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'envoi d'une demande HTTP avec authentification de base est une méthode où les identifiants (nom d'utilisateur et mot de passe) sont envoyés dans l'en-tête de la demande HTTP au format base64. Les programmeurs le font pour accéder à des ressources sécurisées sur un serveur ou une API.

## Comment faire:

Vous pouvez utiliser la bibliothèque C++ `cpp-httplib`. D'abord, installez la bibliothèque.

```C++
// Exemple d'envoi d'une requête HTTP GET avec authentification de base
#include <httplib.h>

int main() {
    httplib::Client cli("httpbin.org");

    httplib::Headers headers = {
        { "Authorization", "Basic " + httplib::detail::base64_encode("user:pass") }
    };

    auto res = cli.Get("/basic-auth/user/pass", headers);

    if (res) {
        std::cout << res->status << std::endl;
        std::cout << res->body << std::endl;
    }
}
```

L'exécution de ce code affiche la réponse HTTP à notre demande, y compris le code de statut et le corps de la réponse.

## Plongée en profondeur

Historiquement, l'authentification de base est une méthode simple et largement adoptée pour sécuriser l'accès au web. Cependant, elle présente des inconvénients, tels que la vulnérabilité aux attaques "man-in-the-middle".

Alternativement, "Bearer Token", "Digest Access", et "OAuth" sont d'autres méthodes d'authentification HTTP couramment utilisées.

L'authentification de base envoie les identifiants dans l'en-tête de chaque requête, les exposant potentiellement si la connexion n'est pas sécurisée (HTTPS). De plus, le serveur doit comparer les identifiants fournis avec ceux stockés, généralement dans une base de données, à chaque requête.

## voir également:

1. [cpp-httplib Github](https://github.com/yhirose/cpp-httplib)
2. [RFC 7617 - Authentification de base HTTP](https://tools.ietf.org/html/rfc7617)
3. [Comment fonctionne l'authentification de base](http://www.ietf.org/rfc/rfc2617.txt)