---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ? 

L'analyse d'une date à partir d'une chaîne, c'est convertir un texte en une date réelle. Les programmeurs le font pour manipuler et utiliser efficacement les informations de date à partir de sources textuelles.

## Comment faire :

Voilà comment faire avec la fonction ```strptime``` de PHP :

```PHP
$format_date = 'm/d/Y';
$date_string = '04/27/2021';
$date = DateTime::createFromFormat($format_date, $date_string);
echo $date->format('Y-m-d');
```

Sortie :

```PHP
2021-04-27
```

Avec ce code, nous convertissons une date formatée en 'm/d/Y' dans une chaîne en un objet DateTime.

## Plongeon profond 

A l'origine, la conversion des dates à partir de chaînes était un casse-tête pour les programmeurs car les formats de date peuvent être assez complexes. En 1995, PHP a introduit ```strptime``` pour simplifier cette tâche, utilisant la notion de "masque de format" pour la conversion.

Il existe d'autres manières d'accomplir cette tâche en PHP. Par exemple, en utilisant la fonction ```strtotime```. Cependant, ```strtotime``` a ses limites car il ne reconnaît pas tous les formats de chaînes de date.

La mise en œuvre de l'analyse de date à partir d'une chaîne dépend des besoins spécifiques du programmeur. Si vous pouvez garantir que vos chaînes de date respectent toujours un seul format, ```strptime``` est un excellent choix.

## Voir aussi 

1. Documentation PHP pour ```strptime```: [https://www.php.net/manual/fr/function.strptime.php](https://www.php.net/manual/fr/function.strptime.php)
2. Documentation PHP pour ```strtotime```: [https://www.php.net/manual/fr/function.strtotime.php](https://www.php.net/manual/fr/function.strtotime.php)
3. Article sur la façon dont le traitement des dates a évolué en PHP : [http://www.php-history.net](http://www.php-history.net)
4. Documentation PHP pour la classe ```DateTime```: [https://www.php.net/manual/fr/class.datetime.php](https://www.php.net/manual/fr/class.datetime.php)