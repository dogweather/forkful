---
title:                "Envoyer une requête http avec authentification basique"
html_title:           "Haskell: Envoyer une requête http avec authentification basique"
simple_title:         "Envoyer une requête http avec authentification basique"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi

L'envoi d'une demande HTTP avec une authentification de base est un moyen sécurisé de protéger les informations sensibles lors de l'accès à une ressource en ligne. Cela peut être particulièrement utile pour les applications qui nécessitent des identifiants de connexion pour accéder à des données confidentielles.

## Comment faire

Voici un exemple simple de code en Haskell pour envoyer une demande GET avec une authentification de base:

```Haskell
import Network.HTTP
import Network.HTTP.Headers
import Network.HTTP.Auth

url = "https://example.com/api" -- Remplacez par votre URL cible 
username = "mon_nom_d_utilisateur" -- Remplacez par votre nom d'utilisateur 
password = "mon_mot_de_passe" -- Remplacez par votre mot de passe

-- Créez une nouvelle demande avec l'URL cible
request = getRequest url 

-- Ajouter les en-têtes d'authentification de base en spécifiant le nom d'utilisateur et le mot de passe
request' = setHeader (mkHeader HdrAuthorization (basicAuthValue username password)) request

-- Envoyer la demande avec la fonction simpleHTTP et récupérer la réponse
response = simpleHTTP request'

-- Imprimer le code d'état et le corps de la réponse
main = do 
    resp <- response
    putStrLn $ "Code d'état: " ++ show (rspCode resp)
    putStrLn $ "Corps de la réponse: " ++ rspBody resp
```

Voici à quoi pourrait ressembler la sortie lorsque vous exécutez ce code:

```Haskell
Code d'état: 200
Corps de la réponse: {"message": "Bienvenue sur l'API!"}
```

## Plongée en profondeur

Maintenant que nous avons vu un exemple de code pour envoyer une demande HTTP avec une authentification de base, voici quelques éléments à retenir:

- L'authentification de base est basée sur l'utilisation du nom d'utilisateur et du mot de passe de l'utilisateur pour créer une chaîne "nom d'utilisateur: mot de passe" encodée en Base64. Cette chaîne est ensuite transmise dans l'en-tête de la demande pour l'authentification.
- Il est important de noter que l'utilisation de l'authentification de base n'est pas considérée comme sûre car la chaîne encodée en Base64 peut être facilement décodée, ce qui pourrait compromettre la sécurité des informations sensibles. Il est donc recommandé d'utiliser des méthodes d'authentification plus sécurisées telles que OAuth ou JWT.
- Vous pouvez également personnaliser l'en-tête d'authentification de base en utilisant la fonction `basicAuth` au lieu de `basicAuthValue`. Cela vous permet de spécifier un domaine ou un type de sécurité supplémentaire pour l'en-tête.

## Voir aussi

- [Documentation sur l'authentification de base en Haskell](http://hackage.haskell.org/package/network-2.8.0.0/docs/Network-HTTP-Headers.html#v:basicAuth)
- [Tutoriel sur l'envoi de demandes HTTP en Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json)
- [Documentation sur le module Network.HTTP d'Haskell](http://hackage.haskell.org/package/network-2.8.0.0/docs/Network-HTTP.html)