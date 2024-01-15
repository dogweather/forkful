---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "PHP: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Pourquoi
Les requêtes HTTP avec une authentification de base sont couramment utilisées pour accéder à des ressources sécurisées ou pour vérifier l'identité de l'utilisateur avant de lui donner accès à certaines fonctionnalités de l'application.

## Comment faire
Pour envoyer une requête HTTP avec une authentification de base en PHP, vous pouvez utiliser la fonction `curl_init()` et spécifier le nom d'utilisateur et le mot de passe dans les options de la requête. Voici un exemple de code :

```PHP
<?php 
// initialisation de la requête curl
$curl = curl_init();

// URL de la ressource à accéder
$url = 'https://example.com/api/resource';

// options de la requête
$options = array(
    CURLOPT_URL => $url,
    CURLOPT_RETURNTRANSFER => true, // pour récupérer la réponse
    CURLOPT_USERPWD => "username:password" // spécifier le nom d'utilisateur et le mot de passe
);

// configuration de la requête curl
curl_setopt_array($curl, $options);

// exécution de la requête
$response = curl_exec($curl);

// gestion des erreurs
if($response === false){
    echo 'Erreur : ' . curl_error($curl);
}

// affichage de la réponse
echo $response;

// fermeture de la requête curl
curl_close($curl);
```

Lorsque vous exécutez ce code, vous devriez obtenir une réponse de la ressource avec l'authentification de base réussie.

## Plongée en profondeur
L'authentification de base est une méthode simple mais peu sécurisée pour accéder à des ressources. Elle utilise un encodage de base64 pour envoyer le nom d'utilisateur et le mot de passe en clair dans l'en-tête de la requête. Il est recommandé d'utiliser d'autres méthodes d'authentification, telles que l'authentification à base de tokens, pour des raisons de sécurité.

## Voir aussi
- [Documentation officielle de PHP sur les requêtes curl](https://www.php.net/manual/fr/book.curl.php)
- [Article détaillé sur l'authentification de base en HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)