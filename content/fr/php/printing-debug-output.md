---
title:                "PHP: Impression de sortie de débogage"
simple_title:         "Impression de sortie de débogage"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi
La sortie de débogage est un outil essentiel pour tout programmeur PHP. Elle permet d'afficher des informations sur le code en cours d'exécution afin de faciliter le débogage et la résolution des erreurs. Sans la sortie de débogage, il est souvent difficile de comprendre pourquoi un code ne fonctionne pas correctement, en particulier pour les développeurs débutants.

## Comment faire
Pour imprimer une sortie de débogage, il suffit d'utiliser la fonction `echo` en PHP. Par exemple, si vous souhaitez afficher la valeur d'une variable `$nom`, vous pouvez écrire `echo $nom;` et sa valeur sera affichée sur la page.

Une autre méthode courante est d'utiliser la fonction `var_dump`, qui imprime des informations détaillées sur une variable, y compris son type et sa valeur. Vous pouvez également utiliser `print_r` pour afficher des informations sur une variable de manière plus lisible par l'homme.

Voici un exemple de code PHP avec la sortie de débogage utilisant ces différentes fonctions :

```PHP
$nom = "Marie";
echo $nom; // affiche "Marie"
var_dump($nom); // affiche string(5) "Marie"
print_r($nom); // affiche Marie
```

## Plongée en profondeur
Il existe plusieurs autres fonctionnalités et méthodes pour la sortie de débogage en PHP, telles que `error_log`, qui enregistre les erreurs dans un fichier, et `debug_backtrace`, qui affiche la trace d'exécution du code.

Il est également possible d'afficher la sortie de débogage dans la console grâce à la fonction `console.log` en utilisant l'API de débogage dans les navigateurs Web.

N'hésitez pas à utiliser des outils et des extensions de débogage tels que Xdebug et Firebug pour rendre le processus de débogage encore plus efficace.

## Voir aussi
- [Documentation officielle de PHP sur la sortie de débogage](https://www.php.net/manual/fr/function.var-dump.php)
- [Guide du débogage en PHP](https://www.php.net/manual/fr/debugger-introduction.php)
- [Firebug - Outil de débogage pour les développeurs Web](https://getfirebug.com/)