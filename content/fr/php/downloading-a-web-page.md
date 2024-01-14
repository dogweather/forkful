---
title:                "PHP: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Télécharger une page Web est une tâche commune dans le développement Web. Que ce soit pour récupérer des données, effectuer des tests ou simplement explorer le code source d'une page, il est souvent nécessaire de télécharger une page Web. Dans cet article, nous allons vous montrer comment le faire en PHP.

## Comment faire

Pour télécharger une page Web en utilisant PHP, nous allons utiliser la fonction `file_get_contents()`. Cette fonction prend en paramètre l'URL de la page à télécharger et renvoie son contenu sous forme de chaîne de caractères. Voyons un exemple concret :

```PHP
<?php 
$url = "https://www.example.com";
$page = file_get_contents($url);
echo $page; // Affiche le contenu de la page
?>
```

Dans cet exemple, nous avons stocké l'URL de la page cible dans une variable `$url` puis nous avons utilisé la fonction `file_get_contents()` pour télécharger la page et stocker son contenu dans une variable `$page`. Enfin, nous avons affiché le contenu de la page en utilisant `echo`.

Mais que se passe-t-il si la page que nous voulons télécharger contient des paramètres GET ? Dans ce cas, nous devons les inclure dans l'URL. Par exemple :

```PHP
<?php 
$url = "https://www.example.com/search?q=php";
$page = file_get_contents($url);
echo $page; // Affiche le contenu de la page de recherche pour "php"
?>
```

De plus, si nous voulons personnaliser notre requête en ajoutant des en-têtes HTTP ou en utilisant une méthode autre que GET, nous pouvons utiliser la fonction `stream_context_create()`. Elle nous permet de créer un contexte pour notre requête et d'inclure toutes les informations supplémentaires nécessaires. Par exemple :

```PHP
<?php
$url = "https://www.example.com";
$options = array(
    'http' => array(
        'method' => "POST",
        'header' => "Content-Type: application/x-www-form-urlencoded\r\n\r\n",
        'content' => http_build_query(array('key' => 'valeur')) // Les paramètres POST sous forme de chaîne de caractères
    ),
);

$context = stream_context_create($options);
$page = file_get_contents($url, false, $context);
echo $page; // Affiche le contenu de la page
?>
```

## Plongée en profondeur

Lorsque vous utilisez la fonction `file_get_contents()` pour télécharger une page, il est important de noter qu'elle ne suit pas les redirections HTTP. Si la page cible effectue une redirection, vous obtiendrez le code HTML de la page de redirection plutôt que celui de la page cible. Pour suivre les redirections, nous pouvons utiliser la fonction `curl`. Voici un exemple :

```PHP
<?php
$url = "https://www.example.com";

$curl_handle = curl_init();
curl_setopt($curl_handle, CURLOPT_URL, $url);
curl_setopt($curl_handle, CURLOPT_HEADER, 0);
curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, 1);
$page = curl_exec($curl_handle);
curl_close($curl_handle);
echo $page; // Affiche le contenu de la page cible
?>
```

En utilisant la fonction `curl`, nous pouvons également personnaliser nos requêtes en utilisant différentes méthodes HTTP, des en-têtes, des cookies, etc.

## Voir aussi

- [Documentation officielle de PHP sur la fonction `file_get_contents()`](https://www.php.net/manual/fr/function.file-get-contents.php)
- [Documentation officielle de PHP sur la fonction `stream_context_create()`](https://www.php.net/manual/fr/function.stream-context-create.php)
- [Documentation officielle de PHP sur la fonction `curl` ](https://www.php.net/manual/fr/book.curl.php)