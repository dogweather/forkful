---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi?
Télécharger une page Web signifie récupérer le code HTML d'une page web sur votre machine. Les programmeurs font cela pour analyser le contenu de la page, récupérer des informations utiles ou automatiser certaines tâches sur le Web.

## Comment faire:
Pour télécharger une page web en PHP, vous pouvez utiliser la fonction file_get_contents() comme ceci :

```PHP
<?php
    $page = file_get_contents('http://example.com');
    echo $page;
?>
```
L'exemple ci-dessus récupère le code HTML de 'http://example.com' et l'affiche.

## Plongée en profondeur
Historiquement, les programmeurs devaient se connecter à un serveur web via Telnet et demander manuellement une page. Avec l'arrivée des langages de programmation modernes comme PHP, ce processus a été simplifié.

Il existe également d'autres méthodes alternatives pour récupérer le contenu d'une page web en PHP, comme la bibliothèque cURL. cURL offre plus de flexibilité et de contrôle sur les requêtes, et est capable de gérer des situations plus complexes.

```PHP
<?php
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "http://example.com");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$page = curl_exec($ch);
curl_close($ch);
echo $page;
?>
```
La différence est que cURL vous permet de modifier les paramètres de la requête, comme l'ajout d'en-têtes, l'envoi de données POST, et bien d'autres.

## Voir aussi
1. [Documentation PHP: file_get_contents()](https://www.php.net/manual/fr/function.file-get-contents.php) - Pour en savoir plus sur file_get_contents().
2. [Documentation PHP: cURL](https://www.php.net/manual/fr/book.curl.php) - Pour en savoir plus sur la bibliothèque cURL.
3. [HTTP: The Protocol Every Web Developer Must Know](https://code.tutsplus.com/tutorials/http-the-protocol-every-web-developer-must-know-part-1--net-31177) - Pour une meilleure compréhension du protocole HTTP.