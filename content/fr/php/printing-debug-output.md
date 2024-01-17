---
title:                "Sortie de débogage d'impression"
html_title:           "PHP: Sortie de débogage d'impression"
simple_title:         "Sortie de débogage d'impression"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Imprimer des sorties de débogage est une pratique courante parmi les programmeurs pour aider à trouver et corriger les problèmes dans leur code. Cela implique d'afficher des messages ou des valeurs spécifiques à des points désignés dans le code afin de mieux comprendre son exécution et de détecter les erreurs potentielles.

## Comment faire:

Voici un exemple de comment imprimer une valeur de variable en utilisant la fonction `echo` de PHP:

```PHP
$nom = "Jean";
echo "Bonjour " . $nom . "!"; // Affiche "Bonjour Jean!"
```

De même, vous pouvez utiliser la fonction `print_r` pour imprimer le contenu complet d'un tableau:

```PHP
$fruits = array("pomme", "orange", "banane");
print_r($fruits); // Affiche Array ( [0] => pomme [1] => orange [2] => banane )
```

Il est également possible de combiner plusieurs valeurs en utilisant des opérateurs de chaîne pour imprimer des messages plus complets. Par exemple:

```PHP
$age = 25;
echo "J'ai " . $age . " ans."; // Affiche "J'ai 25 ans."
```

## Plongée en profondeur:

La pratique d'imprimer des sorties de débogage remonte aux premiers jours de la programmation informatique et reste l'une des méthodes les plus simples et les plus efficaces pour identifier les erreurs dans le code. Cependant, cela peut aussi conduire à des frustrations lorsque des messages de débogage sont oubliés dans le code en production, ou lorsqu'il y a un grand nombre d'entre eux qui rendent difficile la compréhension des résultats.

Heureusement, il existe des alternatives à l'impression de messages de débogage, telles que l'utilisation de loggers pour enregistrer des informations détaillées sur l'exécution du code, ou l'utilisation de la fonction `var_dump` qui affiche les informations de débogage avec un formatage plus lisible.

Il est important de garder à l'esprit que l'impression de sorties de débogage devrait être réservée aux périodes de développement et de test, et qu'elle devrait être supprimée du code avant qu'il ne soit publié en production.

## Voir aussi:

Pour en savoir plus sur l'impression des sorties de débogage en PHP, voici quelques liens utiles:

- [Guide complet pour le débogage en PHP](https://www.php.net/manual/fr/debugger-intro.php)
- [Documentation de la fonction print_r()](https://www.php.net/manual/fr/function.print-r.php)
- [Documentation de la fonction var_dump()](https://www.php.net/manual/fr/function.var-dump.php)