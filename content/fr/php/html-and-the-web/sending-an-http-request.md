---
date: 2024-01-20 18:00:20.366042-07:00
description: "How to: (Comment faire :) PHP utilise cURL et file_get_contents pour\
  \ envoyer des requ\xEAtes HTTP. Voici un exemple simple avec cURL ."
lastmod: '2024-03-13T22:44:57.874774-06:00'
model: gpt-4-1106-preview
summary: "PHP utilise cURL et file_get_contents pour envoyer des requ\xEAtes HTTP."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## How to: (Comment faire :)
PHP utilise cURL et file_get_contents pour envoyer des requêtes HTTP. Voici un exemple simple avec cURL :

```PHP
<?php
// Initialiser une session cURL
$curl = curl_init('https://api.exemple.com/data');

// Configurer les options de cURL
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);

// Exécuter la requête HTTP et enregistrer la réponse
$response = curl_exec($curl);

// Vérifier s'il y a eu une erreur
if ($error = curl_error($curl)) {
    echo 'Erreur cURL : ' . $error;
} else {
    echo 'Réponse : ' . $response;
}

// Fermer la session cURL
curl_close($curl);
?>
```

Avec `file_get_contents` :

```PHP
<?php
// Configurer les options de contexte HTTP
$options = [
    'http' => [
        'method' => 'GET',
        'header' => 'Accept: application/json'
    ]
];

$context = stream_context_create($options);

// Récupérer le contenu depuis un service web
$response = file_get_contents('https://api.exemple.com/data', false, $context);

if ($response === false) {
    echo 'Erreur file_get_contents';
} else {
    echo 'Réponse : ' . $response;
}
?>
```

## Deep Dive (Plongée en profondeur)
Avant cURL et file_get_contents, la communication HTTP en PHP nécessitait des sockets. cURL supporte plus d'options et protocoles, rendant l'envoi de requêtes HTTP(S) plus puissant. `file_get_contents` est simple pour les requêtes GET, mais est limité pour POST/PUT ou la gestion d'erreurs. cURL offre un contrôle fin avec la possibilité de gérer cookies, authentification, timeouts, etc.

## See Also (Voir aussi)
- Documentation PHP cURL : [php.net/manual/fr/book.curl.php](https://www.php.net/manual/fr/book.curl.php)
- Documentation PHP sur les contextes de flux : [php.net/manual/fr/function.stream-context-create.php](https://www.php.net/manual/fr/function.stream-context-create.php)
- Guide HTTP MDN : [developer.mozilla.org/fr/docs/Web/HTTP](https://developer.mozilla.org/fr/docs/Web/HTTP)
