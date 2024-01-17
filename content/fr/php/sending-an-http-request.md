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

## Qu'est-ce que c'est et pourquoi le faire?
L'envoi d'une requête HTTP est une action courante dans la programmation qui permet aux développeurs de communiquer avec d'autres serveurs Web et d'obtenir des données ou des informations. Cela peut être utile pour accéder à des API, récupérer des données de bases de données et bien plus encore.

## Comment faire:
Voici un exemple de code PHP pour envoyer une requête HTTP à l'API de Twitter et afficher le contenu de la réponse:
```
<?php
// Initialisation de la requête cURL
$curl = curl_init();

// Configuration de l'URL à requêter et d'autres options
curl_setopt_array($curl, [
  CURLOPT_URL => "https://api.twitter.com/1.1/statuses/home_timeline.json",
  CURLOPT_RETURNTRANSFER => true,
  CURLOPT_HTTPHEADER => [
    "Authorization: Bearer YOUR_BEARER_TOKEN_HERE"
  ]
]);

// Exécution de la requête et stockage de la réponse dans une variable
$response = curl_exec($curl);

// Fermeture de la requête cURL
curl_close($curl);

// Affichage du contenu de la réponse
echo $response;
```

## Plongée en profondeur:
Historiquement, les requêtes HTTP étaient envoyées en utilisant la fonction `file_get_contents()` ou `fopen()`, mais ces méthodes sont maintenant obsolètes et la méthode recommandée est d'utiliser la librairie cURL.

Il existe des alternatives à cURL telles que la librairie Guzzle ou l'extension HTTP PHP, mais cURL est généralement préférée en raison de sa bonne documentation et de ses performances élevées.

Pour implémenter une requête HTTP avec cURL, il est important de comprendre les différentes options telles que `CURLOPT_URL`, `CURLOPT_RETURNTRANSFER` et `CURLOPT_HTTPHEADER` qui sont utilisées dans l'exemple ci-dessus. Vous pouvez trouver plus d'informations sur ces options et d'autres dans la documentation officielle de cURL.

## Voir aussi:
- [Documentation officielle de cURL](https://www.php.net/manual/en/book.curl.php)
- [Guzzle documentation](http://docs.guzzlephp.org/)
- [Extension HTTP PHP](https://www.php.net/manual/en/book.http.php)