---
title:                "C++: Envoi d'une demande http avec authentification de base"
simple_title:         "Envoi d'une demande http avec authentification de base"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles un programmeur pourrait utiliser l'envoi de requêtes HTTP avec une authentication de base. Certaines de ces raisons incluent :

- La sécurité : l'authentification de base fournit un moyen simple de protéger les données sensibles lors de la communication entre un client et un serveur. Cela peut être particulièrement utile pour les applications web ou mobiles qui gèrent des données confidentielles telles que des informations personnelles ou financières.
- L'accès contrôlé : en utilisant l'authentification de base, il est possible de restreindre l'accès aux ressources du serveur uniquement aux utilisateurs autorisés. Cela peut être particulièrement utile dans les applications où seuls certains utilisateurs ou groupes doivent être en mesure de récupérer des données spécifiques.

## Comment faire

Pour envoyer une requête HTTP avec l'authentification de base en utilisant C++, vous aurez besoin d'utiliser une bibliothèque de gestion des requêtes HTTP telle que `libcurl` ou `cpp-httplib` et d'effectuer les étapes suivantes :

- Inclure la bibliothèque nécessaire dans votre code.
- Définir les informations d'identification requises (nom d'utilisateur et mot de passe) dans une chaîne de caractères.
- Définir l'URL de la ressource à laquelle vous souhaitez accéder dans une autre chaîne de caractères.
- Utiliser la méthode appropriée de la bibliothèque pour effectuer une requête GET, POST ou toute autre méthode HTTP en incluant les informations d'authentification dans les en-têtes de la requête.
- Gérer la réponse du serveur et traiter les données selon vos besoins.

Voici un exemple de code en utilisant la bibliothèque `cpp-httplib` pour envoyer une requête GET avec l'authentification de base :

```C++
#include <httplib.h>
#include <iostream>

int main()
{
    // Création d'une instance de la bibliothèque
    httplib::Client client("www.example.com");

    // Définition des informations d'authentification
    std::string username = "mon_nom";
    std::string password = "mon_mot_de_passe";
    std::string credentials = username + ":" + password;

    // Définition de l'URL de la ressource
    std::string url = "/data/";

    // Envoi de la requête GET avec les informations d'authentification dans les en-têtes
    auto res = client.Get(url.c_str(), {{"Authorization", credentials.c_str()}});

    // Vérification du code de réponse HTTP
    if (res && res->status == 200)
    {
        // Traitement de la réponse
        std::cout << "Récupération des données réussie !\n";
    }
    else
    {
        std::cout << "Erreur lors de la récupération des données.\n";
    }

    return 0;
}
```

Exemple de sortie :

```
Récupération des données réussie !
```

## Approfondissement

L'authentification de base est une méthode d'authentification très simple qui utilise une paire de nom d'utilisateur et mot de passe pour vérifier l'identité de l'utilisateur. Lorsqu'un client envoie une requête HTTP avec l'authentification de base, il ajoute un en-tête "Authorization" à la requête qui contient les informations d'identification encodées en Base64. Le serveur vérifie ensuite ces informations par rapport à une liste d'utilisateurs et de mots de passe autorisés.

Bien que l'authentification de base soit simple à utiliser, elle présente certaines limites en termes de sécurité. Les informations d'identification étant encodées en Base64 et non chiffrées, elles peuvent être facilement décodées par un attaquant. De plus, l'utilisation de cette méthode d'authentification ne fournit pas de protection contre les attaques de type "replay" où un attaquant peut capturer et réutiliser une requête contenant les mêmes informations d'authentification.

## Voir aussi

Pour en apprendre davantage sur l'utilisation de l'authentification de base dans les requêtes HTTP en C++, consultez les ressources suivantes :

- [Documentation officielle de `libcurl`](https://curl.haxx.se/libcurl/c/http-auth.html)
- [