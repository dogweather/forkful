---
title:                "PHP: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi 
    
Les arguments de ligne de commande sont un outil utile pour les développeurs PHP qui souhaitent exécuter leur code en dehors d'un environnement de développement. Ils peuvent être utilisés pour passer des valeurs différentes à votre programme en fonction de la façon dont vous le lancez. Dans cet article de blog, nous allons plonger dans la façon de lire et de traiter les arguments de ligne de commande en PHP.

## Comment Faire 

La première étape pour lire les arguments de ligne de commande en PHP consiste à utiliser la fonction `getopt()` qui prendra les arguments fournis et les convertira en un tableau associatif. Regardons un exemple de code :

```PHP
// En supposant que nous exécutions `php myprogram.php -f file.txt -l`

$options = getopt("f:l:"); // Définit les options à lire sous forme de chaîne de caractères

if (isset($options['f'])) {
    echo "Le fichier que vous avez spécifié est : " . $options['f']; // Sortie : Le fichier que vous avez spécifié est : file.txt
}

if (isset($options['l'])) {
    echo "Vous avez spécifié la langue : " . $options['l']; // Sortie : Vous avez spécifié la langue : fr
}
```

Nous pouvons voir que nous utilisons la fonction `getopt()` pour définir nos options à lire, en utilisant une lettre unique pour chaque option (dans cet exemple, `f` pour le fichier et `l` pour la langue). Ensuite, nous utilisons une simple instruction `if` pour vérifier si ces options ont été spécifiées et pour afficher les valeurs correspondantes.

Vous pouvez également utiliser la variable `$argv` pour accéder aux arguments de ligne de commande, qui sera un tableau contenant tous les arguments fournis lors de l'exécution du script.

## Plongez plus Profondément 

Il existe de nombreuses autres options et fonctions disponibles pour lire les arguments de ligne de commande en PHP, telles que `readline()`, `getopt_long()`, et `$_SERVER['argv']`. Il est important de comprendre que la syntaxe des arguments peut également varier en fonction du système d'exploitation, donc il est recommandé de consulter la documentation officielle de PHP pour une compréhension plus approfondie.

## Voir Aussi

- [Documentation officielle de PHP sur la fonction getopt()](https://www.php.net/manual/fr/function.getopt.php)
- [Article sur la lecture des arguments de ligne de commande en PHP](https://www.phptpoint.com/php-command-line-arguments/)
- [Vidéo tutoriel sur les arguments de ligne de commande en PHP](https://www.youtube.com/watch?v=40qNckK6Ifs)