---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:37.859530-07:00
description: "L'analyse (parsing) du HTML en PHP consiste \xE0 extraire des informations\
  \ sp\xE9cifiques des documents HTML. Les programmeurs r\xE9alisent cette t\xE2che\
  \ pour\u2026"
lastmod: '2024-03-11T00:14:31.826400-06:00'
model: gpt-4-0125-preview
summary: "L'analyse (parsing) du HTML en PHP consiste \xE0 extraire des informations\
  \ sp\xE9cifiques des documents HTML. Les programmeurs r\xE9alisent cette t\xE2che\
  \ pour\u2026"
title: Analyse Syntaxique du HTML
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'analyse (parsing) du HTML en PHP consiste à extraire des informations spécifiques des documents HTML. Les programmeurs réalisent cette tâche pour automatiser l'extraction de données, le web scraping, ou pour intégrer du contenu de diverses pages web dans leurs applications, améliorant ainsi les fonctionnalités sans intervention manuelle.

## Comment faire :
Pour analyser le HTML, les programmeurs PHP peuvent utiliser des fonctions intégrées ou se reposer sur des bibliothèques robustes comme Simple HTML DOM Parser. Ici, nous allons explorer des exemples en utilisant à la fois la classe `DOMDocument` de PHP et le Simple HTML DOM Parser.

### Utilisation de `DOMDocument` :
La classe `DOMDocument` de PHP fait partie de son extension DOM, ce qui permet d'analyser et de manipuler des documents HTML et XML. Voici un exemple rapide sur comment utiliser `DOMDocument` pour trouver toutes les images dans un document HTML :

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Page d'exemple</title>
</head>
<body>
    <img src="image1.jpg" alt="Image 1">
    <img src="image2.jpg" alt="Image 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

Exemple de sortie:
```
image1.jpg
image2.jpg
```

### Utilisation de Simple HTML DOM Parser :
Pour des tâches plus complexes ou une syntaxe plus simple, vous pourriez préférer utiliser une bibliothèque tierce. Simple HTML DOM Parser est un choix populaire, fournissant une interface similaire à jQuery pour naviguer et manipuler des structures HTML. Voici comment l'utiliser :

D'abord, installez la bibliothèque en utilisant Composer :
```
composer require simple-html-dom/simple-html-dom
```

Ensuite, manipulez le HTML pour, par exemple, trouver tous les liens :

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

Ce fragment de code va rechercher le contenu HTML de 'http://www.example.com', l'analyser, et imprimer tous les hyperliens. N'oubliez pas de remplacer `'http://www.example.com'` par l'URL réelle que vous souhaitez analyser.

En utilisant ces méthodes, les développeurs PHP peuvent efficacement analyser le contenu HTML, personnaliser l'extraction de données selon leurs besoins, ou intégrer de manière transparente du contenu web externe dans leurs projets.
