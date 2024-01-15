---
title:                "Envoi d'une requête http"
html_title:           "PHP: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Envoyer une requête HTTP est un moyen essentiel pour communiquer avec des serveurs distants. Cela peut être utile pour récupérer des données de sites web, pour envoyer des données vers des API ou pour réaliser des actions en arrière-plan.

## Comment Faire

Pour envoyer une requête HTTP en PHP, vous pouvez utiliser la fonction `file_get_contents()`. Elle prend en paramètre l'URL cible et retourne le contenu de la réponse de la requête. Par exemple, pour récupérer le contenu d'un site web, vous pouvez utiliser ce code :

```PHP
<?php
$url = "https://www.example.com";
$response = file_get_contents($url);
echo $response;
?>
```

Cela affichera le code HTML du site dans votre navigateur. Si vous souhaitez envoyer une requête avec des paramètres, vous pouvez les inclure dans l'URL comme ceci :

```PHP
<?php
$url = "https://www.example.com/api?param1=valeur1&param2=valeur2";
$response = file_get_contents($url);
echo $response;
?>
```

Il est également possible d'utiliser la fonction `curl` en PHP pour envoyer des requêtes HTTP. Elle offre plus de possibilités de configuration et est utilisée par la plupart des librairies d'API. Voici un exemple de code pour envoyer une requête POST en utilisant `curl` :

```PHP
<?php
$url = "https://www.example.com/api";
$data = array('param1' => 'valeur1', 'param2' => 'valeur2');
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_POSTFIELDS, $data);
$response = curl_exec($ch);
curl_close($ch);
echo $response;
?>
```

## Plongée Profonde

En utilisant la fonction `file_get_contents()`, vous ne pouvez envoyer que des requêtes avec la méthode GET. Pour utiliser d'autres méthodes (POST, PUT, DELETE, etc.), il est nécessaire d'utiliser la fonction `stream_context_create()` en passant un tableau de paramètres en option.

Il est également important de prendre en compte les headers de la requête. Vous pouvez les spécifier en utilisant la fonction `stream_context_create()` ou avec la fonction `curl_setopt()` pour `curl`.

Enfin, assurez-vous de toujours vérifier le code de réponse de la requête pour vous assurer qu'elle s'est correctement déroulée. En général, les codes 2xx indiquent une réponse réussie, les codes 4xx une erreur client et les codes 5xx une erreur serveur.

## Voir Aussi

- Documentation officielle de PHP sur les requêtes HTTP : https://www.php.net/manual/en/book.http.php
- Tutoriel sur l'utilisation de `curl` en PHP : https://www.geeksforgeeks.org/php-curl/
- Documentation officielle de `curl` : https://curl.haxx.se/libcurl/php/