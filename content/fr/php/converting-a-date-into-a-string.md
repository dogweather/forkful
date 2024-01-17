---
title:                "Convertir une date en chaîne de caractères"
html_title:           "PHP: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Convertir une date en chaîne de caractères en PHP est une fonctionnalité commune pour les programmeurs. Cela permet de transformer une date en un format lisible pour les utilisateurs, tel qu'un jour de la semaine ou un mois.

Les programmeurs le font car il est beaucoup plus facile pour les utilisateurs de lire une date sous forme de chaîne de caractères plutôt que de lire une valeur numérique. De plus, cela permet un affichage plus convivial pour les différentes langues et formats de date.

## Comment:

Voici un exemple simple de conversion d'une date en chaîne de caractères en utilisant la fonction `date()` :

```PHP
$date = date("d/m/Y", time()); 
echo $date; // affichera la date actuelle sous forme de chaîne de caractères (ex: 12/12/2019)
```

On peut également utiliser la fonction `strftime()` pour personnaliser le format de la chaîne de caractères selon la langue et les préférences locales :

```PHP
setlocale(LC_TIME, 'fr_FR'); // définir la langue à utiliser (ici: français)
$date = strftime("%A %d %B %Y", time()); 
echo $date; // affichera la date actuelle sous forme de chaîne de caractères en français (ex: Jeudi 12 Décembre 2019)
```

## Plongée en profondeur:

Historiquement, la conversion de dates en chaînes de caractères a été un défi pour les programmeurs en raison de la variation des formats de date à travers le monde. Cependant, grâce aux fonctions `date()` et `strftime()` incluses dans PHP, cela est désormais plus facile à gérer.

Il existe également d'autres méthodes pour convertir des dates en chaînes de caractères, telles que l'utilisation de bibliothèques externes ou de fonctions personnalisées. Cependant, les fonctions intégrées de PHP sont souvent suffisantes pour répondre aux besoins des programmeurs.

## À voir également:

- Documentation officielle de PHP pour les fonctions `date()` et `strftime()`: https://www.php.net/manual/en/function.date.php, https://www.php.net/manual/en/function.strftime.php 
- Tutoriel sur la manipulation des dates en PHP: https://www.w3schools.com/php/php_date.asp