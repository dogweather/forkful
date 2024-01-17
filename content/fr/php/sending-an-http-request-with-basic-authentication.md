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

## Qu'est-ce que c'est et pourquoi le faire ?

Envoyer une requête HTTP avec une authentification de base est un moyen pour les programmeurs de se connecter à un serveur distant et d'accéder à des données protégées. Cela peut être utile lorsque vous travaillez sur une application qui nécessite l'accès à des informations sensibles telles que des données utilisateurs.

## Comment faire :

```PHP
// Utilisation de la fonction curl_init pour initialiser la session
$ch = curl_init();

// Définition de l'URL de destination
curl_setopt($ch, CURLOPT_URL, "www.exemple.com/api/protected_data");

// Ajout des informations d'authentification de base au header de la requête
curl_setopt($ch, CURLOPT_HTTPHEADER, array('Authorization: Basic ' . base64_encode("username:password")));

// Exécution de la requête
$result = curl_exec($ch);

// Fermeture de la session
curl_close($ch);

// Affichage des données protégées
echo $result;
```

## Plongée en profondeur :

L'authentification de base est l'une des méthodes les plus anciennes pour sécuriser les transmissions de données entre un client et un serveur. Elle est basée sur l'échange d'un nom d'utilisateur et d'un mot de passe en clair, qui sont ensuite encodés en utilisant le codage base64. Bien qu'elle soit facile à mettre en place, cette méthode n'est pas considérée comme très sécurisée car les informations d'authentification peuvent être facilement interceptées.

Des alternatives plus avancées telles que l'authentification digest ou l'utilisation de tokens sont maintenant couramment utilisées pour plus de sécurité.

## Voir aussi :

- [Documentation officielle de PHP sur cURL](https://www.php.net/manual/fr/book.curl.php)
- [Article sur les différentes méthodes d'authentification HTTP](https://www.pingidentity.com/en/company/blog/posts/2014/http-basic-authentication.html)
- [Exemples de requêtes HTTP avec authentification de base en PHP](https://www.php.net/manual/fr/function.curl-setopt.php#example-3840)