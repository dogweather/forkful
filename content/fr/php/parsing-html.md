---
title:                "PHP: Analyse de html"
simple_title:         "Analyse de html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/parsing-html.md"
---

{{< edit_this_page >}}

## Pourquoi

Le parsing HTML est une compétence importante pour tout développeur PHP. Cela permet de récupérer et de manipuler les données à partir d'un site web, ce qui peut être utile dans une variété de projets tels que le scraping de données ou la création de bots. Comprendre comment analyser et extraire des informations d'un code HTML peut vous faire gagner du temps et vous rendre plus efficace dans votre travail.

## Comment faire

Pour analyser du HTML en PHP, vous pouvez utiliser la fonction intégrée `file_get_contents` pour récupérer le contenu d'une page web et la fonction `preg_match_all` pour extraire les données spécifiques que vous recherchez. Par exemple, si vous voulez récupérer tous les titres d'un site web, vous pouvez utiliser la regex `/<title>(.*?)<\/title>/` avec `preg_match_all`.

```PHP
$website = file_get_contents("https://example.com");
preg_match_all("/<title>(.*?)<\/title>/", $website, $matches);
print_r($matches[1]);
```

Cela fonctionne pour une seule page, mais si vous voulez parcourir plusieurs pages, vous pouvez utiliser une boucle ou une fonction récursive pour automatiser le processus.

## Plongée en profondeur

Il est important de noter que le parsing HTML peut être complexe car il faut tenir compte de différentes variations de code et de mise en forme. De plus, certains sites peuvent limiter l'accès à leur contenu en utilisant des techniques de protection telles que les cookies ou les en-têtes d'authentification. Dans ces cas, vous devrez peut-être utiliser des bibliothèques externes comme cURL pour simuler une interaction avec le site afin de pouvoir récupérer les données.

Il est également important de comprendre que le parsing HTML peut être sujet à des erreurs et qu'il est crucial de toujours valider et nettoyer les données récupérées avant de les utiliser. Cela peut être fait en utilisant des fonctions telles que `filter_var` pour valider les données selon un format spécifique ou `strip_tags` pour supprimer les balises HTML potentiellement dangereuses.

## Voir aussi

- [Documentation officielle de PHP sur file_get_contents](https://www.php.net/manual/fr/function.file-get-contents.php)
- [Documentation officielle de PHP sur preg_match_all](https://www.php.net/manual/fr/function.preg-match-all)
- [Guide du scraping de données en PHP](https://www.1and1.fr/digitalguide/passer-passer-duzz-syzpwwwn-3-5/programmation/tutoriel-programmation-web-scraper-en-php/)