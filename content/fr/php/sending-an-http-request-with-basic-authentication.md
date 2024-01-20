---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'envoi d'une requête HTTP avec authentification de base est une technique pour accéder aux ressources protégées d'un site web. Les développeurs l'utilisent pour maintenir la sécurité tout en interagissant avec les API web.

## Comment faire :

Voici un simple exemple d'envoi d'une requête HTTP avec authentification de base utilisant l'extension PHP cURL :
```PHP
<?php
$curl = curl_init();

curl_setopt($curl, CURLOPT_URL, "https://example.com");
curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($curl, CURLOPT_USERPWD, "username:password");

$result = curl_exec($curl);
if(!$result){
    die("Error: ".curl_error($curl));
}
curl_close($curl);
echo $result;
?>
```
Le résultat de ce script sera la réponse du serveur à votre requête HTTP.

## Plongée en profondeur :

Historiquement, l'authentification de base HTTP a été proposée avec la spécification HTTP/1.0 comme une méthode simple pour contrôler l'accès aux ressources web.

Comme alternative, vous pourriez envisager l'authentification Digest, les jetons d'authentification Bearer ou même l'authentification OAuth2, selon le niveau de sécurité requis.

Lorsque vous envoyez une requête HTTP avec authentification de base en PHP, le nom d'utilisateur et le mot de passe sont transmis en clair, encodés en Base64, ce qui n'est pas une méthode hautement sécurisée. Il est fortement recommandé de les utiliser sur une connexion HTTPS.

## Voir aussi :

1. [Documentation officielle PHP cURL](https://php.net/manual/en/book.curl.php)
2. [Authentification HTTP sur MDN Web docs](https://developer.mozilla.org/fr/docs/Web/HTTP/Authentication)
3. [Méthodes d'authentification sécurisée alternatives](https://oauth.net/2/)
4. [Base64 sur Wikipedia](https://fr.wikipedia.org/wiki/Base64)