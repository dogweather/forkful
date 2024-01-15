---
title:                "Téléchargement d'une page web."
html_title:           "PHP: Téléchargement d'une page web."
simple_title:         "Téléchargement d'une page web."
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Télécharger une page web peut sembler banal, mais c'est en fait une tâche importante pour tout développeur web. Cela permet de récupérer des informations et des données à utiliser plus tard dans un projet, de tester le bon fonctionnement d'une page et même d'effectuer des tâches d'automatisation.

## Comment faire

Pour télécharger une page web en PHP, il existe différentes méthodes en fonction de vos besoins. Voici quelques exemples :

```PHP
// Utilisation de la fonction file_get_contents()
$url = "https://www.example.com";
$page = file_get_contents($url);
echo $page;

// Utilisation de cURL
$url = "https://www.example.com";
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$page = curl_exec($ch);
curl_close($ch);
echo $page;
```

Le premier exemple utilise la fonction native `file_get_contents()`, qui récupère le contenu d'une URL et le stocke dans une variable. Le second exemple utilise cURL, une bibliothèque qui permet de récupérer des ressources à partir d'URL. Dans les deux cas, le contenu de la page web est stocké dans une variable `$page` et peut être manipulé selon vos besoins.

## Plongez plus profondément

Pour des tâches plus avancées, vous pouvez également utiliser des bibliothèques et des outils dédiés au téléchargement de pages web en PHP. Par exemple, la bibliothèque Guzzle offre une API plus complète et flexible pour l'accès aux ressources en ligne, tandis que l'outil PhantomJS permet de récupérer des pages web avec JavaScript actif.

En outre, il est important de comprendre les différentes méthodes permettant de manipuler le contenu téléchargé, telles que l'utilisation de flux de données ou de fonctions de traitement de chaînes de caractères.

## Voir aussi

- [Bibliothèque Guzzle](https://docs.guzzlephp.org/en/stable/)
- [PhantomJS](https://phantomjs.org/)
- [Documentation officielle de PHP sur les flux de données](https://www.php.net/manual/fr/wrappers.php.php)