---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:32:45.133834-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Traduire le HTML, c'est fouiller dans le markup pour en extraire des données. On le fait pour gratter des infos sur le web, manipuler du contenu, ou intégrer des services tiers.

## How to:
Voici un bout de code PHP qui utilise `DOMDocument` pour parser du HTML et récupérer tous les liens:

```php
<?php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Exemple</title>
</head>
<body>
    <a href="https://example.com">Lien 1</a>
    <a href="https://example.com/deux">Lien 2</a>
</body>
</html>
HTML;

$dom = new DOMDocument();
@$dom->loadHTML($html);
$links = $dom->getElementsByTagName('a');

foreach ($links as $link){
    echo $link->getAttribute('href') . PHP_EOL;
}
?>
```

Sortie :
```
https://example.com
https://example.com/deux
```
## Deep Dive:
Le parsing de HTML existe depuis les débuts du web. Avant `DOMDocument`, on utilisait des expressions régulières (pas l'idéal). Il y a aussi des libs comme `SimpleXML` et `DOMXPath` pour d'autres approches. Dans l'implémentation, on charge le HTML, on le traverse et on extrait ce qu'on veut.

## See Also:
- Documentation PHP sur DOMDocument: [php.net/manual/fr/class.domdocument.php](https://www.php.net/manual/fr/class.domdocument.php)
- SimpleXML pour lire du XML simplement: [php.net/manual/fr/book.simplexml.php](https://www.php.net/manual/fr/book.simplexml.php)
- Tutoriel sur l'utilisation de DOMXPath: [php.net/manual/fr/class.domxpath.php](https://www.php.net/manual/fr/class.domxpath.php)