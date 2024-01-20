---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/parsing-html.md"
---

{{< edit_this_page >}}

# Plongée dans le Parsing HTML avec PHP 

## Quoi & Pourquoi?
Le parsing HTML est le processus d'analyse syntaxique d'un code HTML afin de le comprendre et de pouvoir l'interagir avec lui. Les programmeurs font du parsing HTML pour extraire des informations, manipuler des pages web ou créer du contenu dynamique.

## Comment faire :
Pour parser du HTML avec PHP, nous utilisons la classe `DOMDocument`. Voyons comment faire :

```PHP
<?php
$dom = new DOMDocument;

// Charger HTML à partir d'une chaîne
$dom->loadHTML('<div id="hello">Bonjour le monde!</div>');

// Trouver et afficher l'élément div
$divs = $dom->getElementsByTagName('div');
foreach ($divs as $div) {
    echo $div->nodeValue, PHP_EOL;
}
```
Résultat :
```
Bonjour le monde!
```
## Plongée Profonde :
Historiquement, les développeurs utilisaient les expressions régulières pour analyser le HTML. Cependant, ce n'est pas la solution idéale car le HTML a une structure d'arbre, pas linéaire. 

Pour le parsing HTML, PHP propose aussi SimpleXML qui est plus facile a utiliser mais moins puissant que DOMDocument. 

En ce qui concerne l'implémentation, la méthode `loadHTML()` de la classe `DOMDocument` utilise libxml pour analyser la chaîne HTML en un arbre DOM.

## Voir Aussi :

- Documentation sur la Manipulation de documents XML - PHP: https://www.php.net/manual/fr/book.dom.php
- Tutoriel PHP SimpleXML : https://www.php.net/manual/fr/book.simplexml.php
- Guide de libxml - XML C parser and toolkit: http://xmlsoft.org/