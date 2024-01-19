---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi? 

L'impression de sortie de débogage est une méthode de détection des erreurs rencontrées lors de l'exécution d'un programme. Les programmeurs l'utilisent pour comprendre l'état de leur programme à un moment donné et résoudre les problèmes plus efficacement.

## Comment faire:
Pour imprimer du texte brut en PHP, on utilise la fonction `echo` ou `print`. Voyez ci-dessous :

```PHP
<?php
$data = "Nous sommes en train de déboguer";
echo $data;
print($data);
?>
```
Cela donnera la sortie, "Nous sommes en train de déboguer". Pour le débogage proprement dit, nous utilisons souvent la fonction `var_dump` pour afficher la structure d'une variable. 

```PHP
<?php
$data = array('a', 'b', 'c');
var_dump($data);
?>
```
Cela permettra d'afficher les détails du tableau `$data`.

## Où creuser?
L'expression de commutation en PHP a vu le jour avec PHP 4. C'est un moyen rapide et facile d'afficher de l'information utile pour le débogage. Cependant, dans les contextes de production, il est souvent peu pratique d'afficher directement la sortie de débogage. 

Alternativement, vous pouvez utiliser des outils comme Xdebug ou des bibliothèques comme Monolog pour envoyer votre débogage à un fichier journal ou à un service externe.

Le 'print_r' est une autre fonction couramment utilisée pour l'impression de sortie de débogage en PHP. Contrairement à `var_dump`, `print_r` ne montre pas le type et la taille de la variable.

## Voir Aussi:
Pour plus d'informations sur le débogage en PHP, veuillez consulter:

1. [Déboguer en PHP](https://www.php.net/manual/fr/debugger.php)
2. [Xdebug](https://xdebug.org/docs/)
3. [Monolog](https://github.com/Seldaek/monolog)
4. [La fonction `print_r`](https://www.php.net/manual/fr/function.print-r.php)

Gardez à l'esprit qu'une bonne pratique de débogage facilite grandement le développement et la maintenance de votre code PHP.