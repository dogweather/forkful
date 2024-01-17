---
title:                "Calculer une date dans le futur ou le passé."
html_title:           "PHP: Calculer une date dans le futur ou le passé."
simple_title:         "Calculer une date dans le futur ou le passé."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Calculer une date dans le futur ou le passé est un procédé couramment utilisé par les programmeurs pour déterminer des événements spécifiques en fonction d'une date de référence. Cela peut être utile dans le développement d'applications et de sites web où il est important de planifier des tâches ou de définir des événements dans le temps.

## Comment faire:

Voici un exemple de code en PHP pour calculer une date dans le futur en utilisant la fonction `date_add()` :

```PHP
$current_date = new DateTime(); // Date de référence

// Ajouter 1 mois à la date de référence
date_add($current_date, date_interval_create_from_date_string('1 month'));

echo $current_date->format('Y-m-d'); // Affiche la date dans un format spécifique
```

Output: 2020-08-25 (si la date de référence est 2020-07-25)

Pour calculer une date dans le passé, on peut utiliser la fonction `date_sub()` et spécifier l'interval de temps négatif.

## Plongée en profondeur:

Avant l'avènement du numérique, les calculs de dates se faisaient manuellement ou à l'aide de calendriers. Avec l'utilisation des langages de programmation tels que PHP, il est désormais facile de calculer des dates en fonction de différents paramètres comme les jours ouvrables, les années bissextiles et les fuseaux horaires.

Une alternative à l'utilisation de fonctions PHP pour calculer des dates est l'utilisation de bibliothèques externes telles que Carbon, qui offrent des fonctionnalités plus avancées pour la manipulation de dates.

L'implémentation de la fonction `date_add()` utilise un objet DateTime et une intervalle de temps spécifiée pour ajouter cette période au date de référence. Il est important de noter que la date résultante peut être affectée par les réglages de fuseaux horaires et formats de date spécifiques.

## Voir aussi:

- Documentation officielle de PHP pour les fonctions `date_add()` et `date_sub()`: https://www.php.net/manual/fr/function.date-add.php et https://www.php.net/manual/fr/function.date-sub.php
- Documentation de la bibliothèque Carbon : https://carbon.nesbot.com/
- Autres fonctions utiles de manipulation de dates en PHP : https://www.php.net/manual/fr/ref.datetime.php