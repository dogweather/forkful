---
date: 2024-01-20 17:53:14.443970-07:00
description: 'Comment faire : Pour imprimer quelque chose rapidement, on utilise souvent
  `echo` ou `print` .'
lastmod: '2024-04-05T21:53:59.366455-06:00'
model: gpt-4-1106-preview
summary: Pour imprimer quelque chose rapidement, on utilise souvent `echo` ou `print`
  .
title: "Affichage des sorties de d\xE9bogage"
weight: 33
---

## Comment faire :
Pour imprimer quelque chose rapidement, on utilise souvent `echo` ou `print` :

```PHP
<?php
$variable = "Salut le monde !";
echo $variable; // Affiche "Salut le monde !"
?>
```

Pour du débogage, `var_dump()` et `print_r()` sont super utiles car ils révèlent plus d'infos :

```PHP
<?php
$tableau = array("a" => "pomme", "b" => "banane");
var_dump($tableau);

// Produit quelque chose comme :
// array(2) {
//   ["a"]=>
//   string(5) "pomme"
//   ["b"]=>
//   string(6) "banane"
// }

print_r($tableau);

// Affiche :
// Array
// (
//    [a] => pomme
//    [b] => banane
// )
?>
```

## Petite plongée :
Historiquement, `print` et `echo` existent depuis les premiers jours de PHP, simples et directs pour afficher du texte. `print_r()` est apparu pour donner une représentation lisible à l'humain des variables, surtout pratique pour des tableaux ou des objets simples. `var_dump()` va encore plus loin en affichant aussi les types et tailles de données, essentiel pour déboguer des structures complexes.

Il y a des alternatives modernes comme Xdebug qui permettent un débogage plus avancé avec des breakpoints et un suivi du code pas à pas. Mais pour des vérifications rapides, rien n'est aussi simple que d'ajouter un `var_dump()` dans votre code.

En terme d'implémentation, ces fonctions ont une importance capitale en développement : elles sont souvent retirées ou rendues inactives en production pour éviter des fuites d'informations sensibles.

## Voir également :
- La documentation php.net pour `echo` : https://www.php.net/manual/fr/function.echo.php
- La documentation php.net pour `print` : https://www.php.net/manual/fr/function.print.php
- La documentation php.net pour `print_r` : https://www.php.net/manual/fr/function.print-r.php
- La documentation php.net pour `var_dump` : https://www.php.net/manual/fr/function.var-dump.php
- Site officiel de Xdebug pour des outils de débogage plus sophistiqués : https://xdebug.org
