---
title:                "Afficher la sortie de débogage"
html_title:           "PHP: Afficher la sortie de débogage"
simple_title:         "Afficher la sortie de débogage"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de plonger dans la façon de procéder pour imprimer les sorties de débogage, il est important de comprendre pourquoi cela peut être utile. La plupart du temps, les développeurs utilisent des messages de débogage pour comprendre leur code et identifier les erreurs ou les bogues. Cela peut être particulièrement utile lors du développement de nouvelles fonctionnalités ou de la résolution de problèmes dans un projet.

## Comment faire

Il existe différentes façons d'imprimer des sorties de débogage en PHP en fonction de vos besoins et de votre style de codage. Voici quelques exemples pour vous aider à démarrer :

```PHP
// Imprimer une variable
$nom = "Marie";
echo $nom;

// Imprimer un tableau
$notes = [18, 15, 12];
print_r($notes);

// Utiliser var_dump pour obtenir des informations plus détaillées
$personne = [
  "nom" => "Jean",
  "age" => 30,
  "ville" => "Paris"
];
var_dump($personne);

// Utiliser des marqueurs de position pour afficher des valeurs spécifiques avec printf
$nom = "Sophie";
$ville = "Marseille";
printf('Mon amie %s vient de %s.', $nom, $ville);

// Utiliser la fonction error_log pour envoyer les messages de débogage dans un fichier journal
error_log('Ce message sera enregistré dans le fichier journal du serveur.');
```

Voici un exemple de sortie pour chacune de ces méthodes :

```
Marie
Array ( [0] => 18 [1] => 15 [2] => 12 )
array(3) {
  ["nom"]=>
  string(4) "Jean"
  ["age"]=>
  int(30)
  ["ville"]=>
  string(5) "Paris"
}
Mon amie Sophie vient de Marseille.
```

## Plongée en profondeur

En plus des méthodes mentionnées ci-dessus, il existe également des outils spécifiques à PHP pour l'impression de sorties de débogage comme xdebug et Kint. Ces outils offrent des fonctionnalités avancées telles que la coloration syntaxique, la navigation dans les traces d'exceptions et la visualisation des tableaux et des objets de manière plus claire.

De plus, il est important de garder à l'esprit que l'impression excessive de messages de débogage peut entraîner des performances réduites, en particulier dans les environnements de production. Il est donc conseillé de n'utiliser des sorties de débogage que lorsque cela est nécessaire et de les supprimer une fois que le problème a été résolu.

## Voir aussi

- [Documentation officielle de PHP sur les messages de débogage](https://www.php.net/manual/fr/features.debugging.php)
- [Tutoriel sur l'utilisation de xdebug pour le débogage en PHP](https://www.developpez.net/forums/d2132747/php/langage/debugguer-une-application-xdebug/)
- [Comparaison des différents outils de débogage pour PHP](https://dzone.com/articles/various-php-debugging-tools)