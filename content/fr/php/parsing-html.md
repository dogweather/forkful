---
title:                "Analyse de code html"
html_title:           "PHP: Analyse de code html"
simple_title:         "Analyse de code html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire? 
Le parsing HTML est le processus de lecture et d'analyse du code source d'une page Web pour en extraire des informations ou effectuer des manipulations. Les programmeurs utilisent le parsing HTML pour récupérer des données spécifiques sur une page Web ou pour automatiser certaines tâches telles que la mise à jour de contenu. 

## Comment faire: 
Voici un exemple de code PHP pour parser une page Web et extraire les liens hypertexte présents dans le code source : 

```PHP
$page = file_get_contents("url_de_la_page"); // récupère le code source de la page
$matches = []; // initialise un tableau vide pour stocker les liens
preg_match_all('/<a href=[\"\'](.+)[\"\']>(.+)<\/a>/i', $page, $matches); // recherche et enregistre les liens dans le tableau
foreach($matches[1] as $link){ // parcourt le tableau de liens
    echo $link . "<br>"; // affiche chaque lien trouvé
}
```
Exemple de résultat :
```
https://www.example.com
https://www.example.com/fr/
https://www.example.com/contact
```

## Plongée en profondeur: 
Le parsing HTML a été développé pour permettre aux navigateurs Web d'afficher des pages Web, mais il a depuis été utilisé dans de nombreux domaines tels que le web scraping, le traitement de données et la génération de rapports. Il existe des alternatives au parsing HTML telles que l'utilisation d'API (interfaces de programmation d'application) ou de bibliothèques spécialisées, mais le parsing reste une méthode populaire et efficace. Dans l'exemple précédent, nous avons utilisé la fonction `preg_match_all()` qui utilise des expressions régulières pour rechercher et extraire les liens hypertexte. Il est important de noter que le parsing HTML peut être complexe et nécessite une connaissance approfondie de la syntaxe HTML.

## Voir aussi: 
- [Documentation officielle de PHP sur le parsing HTML](https://www.php.net/manual/fr/domdocument.loadhtml.php)
- [Tutoriel sur le parsing HTML avec PHP](https://www.tutorialspoint.com/php/php_parsing_html.htm)
- [Bibliothèque PHP pour le web scraping](https://github.com/vermilion-tech/scrapercube)