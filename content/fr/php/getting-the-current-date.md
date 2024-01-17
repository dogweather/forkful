---
title:                "Obtenir la date actuelle."
html_title:           "PHP: Obtenir la date actuelle."
simple_title:         "Obtenir la date actuelle."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
La récupération de la date actuelle est une fonctionnalité courante pour les programmeurs PHP. Cela leur permet de manipuler et de comparer des dates à des fins de traitement ou d'affichage. 

## Comment faire:
Pour obtenir la date actuelle en PHP, il suffit d'utiliser la fonction `date()`. Voici un exemple de code pour afficher la date du jour:

```PHP
<?php
echo date("d/m/Y"); //output: 22/04/2021
?>
```

Il est également possible de spécifier un format différent pour la date en utilisant des lettres spéciales. Par exemple, si l'on souhaite afficher la date sous la forme "22 avril 2021", il suffit de modifier le code comme suit:

```PHP
<?php
echo date("d F Y"); //output: 22 avril 2021
?>
```

## Plongée en profondeur:
La récupération de la date actuelle est une fonctionnalité essentielle dans la programmation PHP, car elle permet de manipuler et de comparer des dates en utilisant la fonction `strtotime()` ou des objets `DateTime`. Cela peut être utile pour des tâches telles que la planification d'événements, le calcul de durées ou la vérification de l'âge d'un utilisateur.

En dehors de la fonction `date()`, il existe d'autres moyens d'obtenir la date actuelle en PHP, tels que l'utilisation de la fonction `time()` ou la méthode `now()` d'un objet `DateTime`. Cependant, la fonction `date()` reste la méthode la plus couramment utilisée en raison de sa simplicité et de sa polyvalence.

## Voir aussi:
- [Documentation PHP sur la fonction date()](https://www.php.net/manual/fr/function.date.php)
- [Guide de la syntaxe de la fonction date()](https://www.w3schools.com/php/func_date_date.asp)
- [Tutoriel vidéo sur la récupération de la date actuelle en PHP](https://www.youtube.com/watch?v=uIc4V90e4H4)