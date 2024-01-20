---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Envoyer une requête HTTP c'est simplement demander à un serveur Web d'obtenir ou d'envoyer des données. Nous, les développeurs, faisons cela pour interagir avec des services Web, récupérer des données, envoyer des données, etc.

## Comment faire:

Pour envoyer une requête HTTP en PHP, nous utilisons la bibliothèque cURL ou file_get_contents. Voici un exemple simple :

Avec cURL:

```PHP
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL,"http://www.exemple.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$server_output = curl_exec($ch);
curl_close ($ch);

echo $server_output;
```

Avec file_get_contents:

```PHP
$url = 'http://www.exemple.com';
$data = file_get_contents($url);
echo $data;
```

## Plongée en profondeur:

Historiquement, PHP n'avait pas de support intégré pour les requêtes HTTP. Aujourd'hui, cURL et file_get_contents sont les façons les plus courantes de le faire.

Il existe d'autres manières (comme PECL_HTTP ou httpful), mais elles nécessitent l'installation de modules supplémentaires.

Le choix entre ces options dépend de votre projet spécifique. Par exemple, cURL offre plus de flexibilité et de contrôle sur vos requêtes HTTP, tandis que file_get_contents est une solution plus simple et plus rapide à mettre en œuvre.

## Voir aussi:

Pour plus d'informations, vérifiez ces excellentes ressources :

- Documentation officielle PHP pour cURL: https://www.php.net/manual/fr/book.curl.php
- Documentation officielle PHP pour file_get_contents: https://www.php.net/manual/fr/function.file-get-contents.php
- Un guide pratique de StackOverflow sur les requêtes HTTP en PHP (en anglais) : https://stackoverflow.com/questions/5647461/how-do-i-send-a-post-request-with-php