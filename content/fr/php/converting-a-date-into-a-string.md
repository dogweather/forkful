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

## Pourquoi

Vous pourriez être amené à convertir une date en chaîne de caractères pour afficher une date dans un format spécifique ou pour effectuer des opérations plus complexes. Cela peut également être utile pour stocker une date dans une base de données ou l'afficher dans un fichier de journal.

## Comment faire

```PHP
<?php
// Convertir la date actuelle en une chaîne de caractères dans le format "jour/mois/année"
$date = date('d/m/Y');
echo $date; // Résultat: 02/04/2021
?>
```

Il existe plusieurs options pour formater la date. Voici quelques exemples:

```PHP
<?php
// Convertir la date actuelle en chaîne de caractères dans le format "année-mois-jour heure:minute:seconde"
$date = date('Y-m-d H:i:s');
echo $date; // Résultat: 2021-04-02 10:05:23

// Convertir une date spécifique en chaîne de caractères
$date = date('d/m/Y', strtotime('10 December 2020'));
echo $date; // Résultat: 10/12/2020
?>
```

Il est également possible d'utiliser la fonction `strftime()` pour formater la date en fonction de la langue et de la localisation du serveur.

## Plongée en profondeur

Lorsque vous utilisez la fonction `date()`, vous pouvez utiliser différents caractères pour définir le format de la date:

- `d`: jour du mois, avec un zéro devant si nécessaire (01 à 31)
- `m`: mois, avec un zéro devant si nécessaire (01 à 12)
- `Y`: année sur quatre chiffres, avec des zéros devant si nécessaire (exemple: 2021)
- `H`: heure au format 24 heures, avec des zéros devant si nécessaire (00 à 23)
- `i`: minutes, avec des zéros devant si nécessaire (00 à 59)
- `s`: secondes, avec des zéros devant si nécessaire (00 à 59)

En utilisant ces caractères dans un ordre spécifique, vous pouvez créer votre propre format de date personnalisé.

## Voir aussi

- [Documentation officielle de PHP sur la fonction date()](https://www.php.net/manual/fr/function.date.php)
- [Guide de référence PHP pour formater les dates](https://www.php.net/manual/fr/datetime.format.php)