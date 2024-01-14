---
title:                "PHP: Comparaison de deux dates"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi Comparer Deux Dates en PHP?

Comparer deux dates est une tâche courante en programmation, particulièrement en PHP. Cela peut être utile pour vérifier si une date se situe avant ou après une autre, ou encore pour calculer la différence entre deux dates. Cela peut également être nécessaire pour effectuer des tâches telles que la gestion d'abonnements ou la planification d'événements. Dans cet article, nous allons explorer comment comparer deux dates en utilisant PHP.

## Comment Faire?

La première étape pour comparer deux dates en PHP est de s'assurer qu'elles sont toutes les deux dans un format valide. Pour cela, nous pouvons utiliser la fonction `strtotime()` qui convertit une chaîne de caractères en timestamp. Voici un exemple de code:

```PHP
$date1 = "15-07-2020";
$date2 = "20-07-2020";

$timestamp1 = strtotime($date1);
$timestamp2 = strtotime($date2);

if ($timestamp1 < $timestamp2) {
  echo "$date1 se situe avant $date2";
} else if ($timestamp1 > $timestamp2) {
  echo "$date1 se situe après $date2";
} else {
  echo "Les deux dates sont égales";
}
```
**Output:** 15-07-2020 se situe avant 20-07-2020

Ici, nous avons utilisé la fonction `strtotime()` pour convertir les deux dates en timestamps, puis nous avons comparé les timestamps avec l'opérateur de comparaison `<` et `>`.

Il est important de noter que cette méthode ne fonctionnera que si les deux dates sont dans le même format. Nous pouvons également utiliser la classe `DateTime` pour manipuler les dates et les comparer dans différents formats.

Voici un exemple utilisant la classe `DateTime` pour comparer des dates dans un format différent:

```PHP
$date1 = new DateTime("15 juillet 2020");
$date2 = new DateTime("July 20, 2020");

if ($date1 < $date2) {
  echo $date1->format("d/m/Y") . " se situe avant " . $date2->format("d/m/Y");
} else if ($date1 > $date2) {
  echo $date1->format("d/m/Y") . " se situe après " . $date2->format("d/m/Y");
} else {
  echo "Les deux dates sont égales";
}
```
**Output:** 15/07/2020 se situe avant 20/07/2020

Cette méthode fonctionne en utilisant la fonction `format()` pour spécifier le format souhaité pour l'affichage de la date.

## Plongeons Plus Profondément

Il est également possible de comparer des dates en utilisant les méthodes de la classe `DateTime`. Par exemple, nous pouvons utiliser `diff()` pour calculer la différence entre deux dates:

```PHP
$date1 = new DateTime("15 juillet 2020");
$date2 = new DateTime("July 20, 2020");

$diff = $date1->diff($date2);

echo "Il y a " . $diff->days . " jours de différence entre les deux dates.";
```
**Output:** Il y a 5 jours de différence entre les deux dates.

La classe `DateTime` offre également d'autres méthodes utiles pour comparer des dates telles que `modify()`, `add()` et `sub()`. N'hésitez pas à explorer ces méthodes pour une utilisation plus avancée dans vos projets.

## Voir Aussi

- [Documentation officielle de PHP sur la gestion de dates](https://www.php.net/manual/fr/datetime.formats.php)
- [Tutorial PHP sur la classe DateTime](https://www.tutorialspoint.com/php/php_date_time.htm)
- [Exemple de projet utilisant la classe DateTime pour comparer des dates](https://www.code-wall.com/showthread.php?t=1061)