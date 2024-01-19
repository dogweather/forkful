---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "PHP: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
En programmation, calculer une date dans le futur ou le passé est un processus qui permet de déterminer une date précise en ajoutant ou en soustrayant un nombre défini de jours à une date donnée. Les programmeurs le font souvent pour planifier des événements, programmer des rappels ou gérer des délais.

## Comment faire :
Voici comment on peut faire cela en PHP, en utilisant l'objet `DateTime`:

```PHP
// Créer un objet DateTime
$date = new DateTime('2022-04-01');

// Ajouter 30 jours
$date->modify('+30 days');
echo $date->format('Y-m-d'); 
// Sortie : 2022-05-01

// Soustraire 15 jours
$date->modify('-15 days');
echo $date->format('Y-m-d');
// Sortie : 2022-04-16
```

Voilà, c'est simple comme bonjour !

## Plongée profonde :
Le calcul des dates est une tâche courante dans le développement de logiciels depuis des décennies. Depuis PHP 5.2.0, la classe `DateTime` a rendu cette tâche beaucoup plus facile en PHP. 

En ce qui concerne les alternatives, vous pouvez aussi utiliser `strtotime()`, mais je recommande `DateTime` pour plus de clarté et de flexibilité. Par exemple :

```PHP
$date = strtotime('2022-04-01 +30 days');
echo date('Y-m-d', $date);
// Sortie : 2022-05-01
```

L'implémentation du calcul de la date en PHP prend en compte les années bissextiles, de sorte que l'ajout ou la soustraction de jours donne toujours une date correcte.

## Voir aussi :
- [PHP: DateTime - Manual](https://www.php.net/manual/fr/class.datetime.php)
- [PHP: strtotime - Manual](https://www.php.net/manual/fr/function.strtotime.php)
- [PHP: date - Manual](https://www.php.net/manual/fr/function.date.php) 

C'est tout ce que vous avez besoin de savoir pour gérer les dates en PHP. Allez-y, essayez-le vous-même !