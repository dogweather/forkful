---
title:                "PHP: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle peut sembler une tâche simple, mais c'est une étape importante dans de nombreux projets de programmation PHP. En utilisant une variété de fonctions et de formats de date, vous pouvez afficher la date actuelle sur votre site web ou application et la mettre à jour en temps réel.

## Comment Faire

```PHP
// Afficher la date actuelle au format jour/mois/année
echo date('d/m/Y');

// Afficher la date et l'heure actuelles au format 24 heures
echo date('d/m/Y H:i:s');

// Afficher la date actuelle avec un fuseau horaire spécifique (ici, Paris)
$date = new \DateTime('now', new \DateTimezone('Europe/Paris'));
echo $date->format('d/m/Y H:i:s');
```

### Explications

La fonction `date()` en PHP permet d'afficher la date actuelle dans différents formats en utilisant des caractères spéciaux tels que `d` pour le jour, `m` pour le mois et `Y` pour l'année. Vous pouvez également afficher l'heure en utilisant des caractères comme `H` pour les heures, `i` pour les minutes et `s` pour les secondes.

Pour afficher la date et l'heure avec un fuseau horaire spécifique, nous utilisons la classe `DateTime` et la méthode `format()` pour spécifier le format de date souhaité.

## Approfondissement

La fonction `date()` utilise le fuseau horaire du serveur pour déterminer la date et l'heure actuelles. Pour afficher la date et l'heure dans un fuseau horaire différent, vous pouvez modifier les paramètres de votre serveur ou utiliser la méthode `setTimezone()` de la classe `DateTime`.

Il est également possible d'obtenir la date et l'heure à partir d'une date spécifique ou en utilisant des formats de date personnalisés. Vous pouvez en apprendre plus sur ces fonctionnalités en consultant la documentation officielle de PHP.

## Voir Aussi

- [Documentation PHP sur la fonction `date()`](https://www.php.net/manual/fr/function.date.php)
- [Documentation PHP sur la classe `DateTime`](https://www.php.net/manual/fr/class.datetime.php)
- [Formats de date disponibles en PHP](https://www.php.net/manual/fr/datetime.format.php)