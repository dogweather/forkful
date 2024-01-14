---
title:    "PHP: Impression de sortie de débogage"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur, vous avez probablement rencontré des problèmes lors du débogage de votre code. Souvent, vous pouvez passer des heures à essayer de trouver la cause d'un bug sans succès. C'est là que l'impression de sorties de débogage peut être utile. Cela permet d'obtenir des informations sur le comportement de votre code à différents endroits, ce qui peut vous aider à identifier plus facilement la source du problème.

## Comment faire

Pour imprimer des sorties de débogage en PHP, vous pouvez utiliser la fonction `print_r()` ou `var_dump()`, qui affichent toutes deux les informations sur une variable, un objet ou un tableau. Par exemple :

```PHP
$variable = 42;
$array = [1, 2, 3];

// Imprime la valeur de la variable
print_r($variable);

// Imprime le contenu du tableau
var_dump($array);
```

Ces fonctions peuvent également être utilisées à l'intérieur de boucles ou de conditions pour afficher des informations spécifiques à un moment précis dans votre code.

## Plongée en profondeur

En plus des fonctions de base mentionnées ci-dessus, il existe également des outils plus avancés pour l'impression de sorties de débogage en PHP. Par exemple, vous pouvez utiliser l'extension Xdebug qui offre des fonctionnalités telles que le traçage de l'exécution du code et la mise en surbrillance des erreurs syntaxiques.

Il existe également des outils de débogage visuel, tels que phpDebugger, qui vous permettent de visualiser les informations de débogage directement dans votre navigateur.

Dans tous les cas, assurez-vous de supprimer ou de désactiver vos sorties de débogage avant de publier votre code en production, sinon cela pourrait révéler des informations sensibles sur votre site ou votre application.

## Voir aussi

- [Documentation officielle PHP sur le débogage](https://www.php.net/manual/fr/debugger.php)
- [Extension PHP Xdebug](https://xdebug.org/)
- [phpDebugger](https://phpdebugger.com/)