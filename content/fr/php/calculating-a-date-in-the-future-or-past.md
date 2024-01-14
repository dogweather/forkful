---
title:                "PHP: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi
Calculer une date dans le futur ou dans le passé peut être utile dans de nombreuses situations en programmation. Cela peut vous aider à planifier des tâches à l'avance, à afficher des informations de manière dynamique ou à effectuer des opérations arithmétiques sur des dates.

## Comment faire
Pour calculer une date dans le futur ou dans le passé en PHP, vous pouvez utiliser la fonction `strtotime()`. Cette fonction prend en paramètres une chaîne de caractères représentant la date de départ et un nombre de secondes à ajouter ou à soustraire.

```PHP
$date = strtotime("now"); //Date actuelle
echo date("d/m/Y", $date); //Affichage au format jour/mois/année

$date_demain = strtotime("+1 day"); //Date de demain
echo date("d/m/Y", $date_demain);

$date_mois_prochain = strtotime("+1 month"); //Date du mois prochain
echo date("d/m/Y", $date_mois_prochain);
```

L'exemple ci-dessus utilise la fonction `date()` pour formater la date en jour/mois/année. Vous pouvez également utiliser `strtotime()` pour calculer des dates dans le passé en utilisant un nombre négatif comme paramètre.

## Plongée profonde
En utilisant la fonction `strtotime()`, vous pouvez effectuer des calculs plus avancés sur des dates. Par exemple, vous pouvez ajouter ou soustraire des semaines, des mois ou même des années.

```PHP
$date = strtotime("+2 weeks 3 days"); //2 semaines et 3 jours à partir de maintenant
echo date("d/m/Y", $date);

$date_prochain_mois = strtotime("next month"); //Date du prochain mois en cours
echo date("d/m/Y", $date_prochain_mois);

$date_prochaine_annee = strtotime("+1 year"); //Date de l'année prochaine
echo date("d/m/Y", $date_prochaine_annee);
```

En plus de cela, la fonction `strtotime()` peut également gérer des chaînes de caractères telles que "tomorrow" (demain), "next Monday" (lundi prochain) ou "last Friday" (vendredi dernier). Cela vous permet de calculer des dates en fonction des jours de la semaine sans avoir à définir une date spécifique.

## Voir aussi
- [Documentation officielle de la fonction strtotime()](https://www.php.net/manual/fr/function.strtotime.php)
- [Explication complète sur les opérations de dates en PHP](https://www.php.net/manual/fr/datetime.formats.relative.php)
- [Tutoriel sur la manipulation de dates en PHP](https://www.w3schools.com/php/php_date.asp)

Merci d'avoir lu cet article sur comment calculer une date dans le futur ou dans le passé en PHP ! N'hésitez pas à explorer d'autres fonctionnalités de la fonction `strtotime()` pour des opérations de dates plus complexes. À bientôt pour plus de tutoriels de programmation !