---
title:    "PHP: Comparer deux dates"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Pourquoi comparer deux dates en PHP ?

Comparer deux dates en PHP est une tâche courante pour de nombreux développeurs. Que ce soit pour vérifier si une date est antérieure ou postérieure à une autre, ou pour afficher des données sur une période spécifique, cela peut être très utile dans de nombreux projets.

# Comment faire pour comparer deux dates en PHP ?

Pour comparer deux dates en PHP, il existe plusieurs fonctions prédéfinies telles que "strtotime" et "date_diff". La première permet de convertir une chaîne de caractères en timestamp, tandis que la seconde permet de calculer la différence entre deux dates et de l'afficher de manière lisible.

Voici un exemple de code en utilisant ces fonctions :

```PHP
$date1 = strtotime("2020-01-01"); // conversion de la chaîne en timestamp
$date2 = strtotime("2020-12-31");

$diff = date_diff($date1, $date2); // calcul de la différence entre les deux dates
echo $diff->format("Il reste %m mois et %d jours avant la fin de l'année."); // affichage du résultat
// output : Il reste 11 mois et 30 jours avant la fin de l'année.
```

En utilisant ces fonctions, vous pouvez effectuer des comparaisons de dates plus complexes en utilisant des opérateurs logiques tels que ">", "<" ou "==". Par exemple :

```PHP
$date1 = strtotime("2020-01-01");
$date2 = strtotime("+1 week"); // ajout d'une semaine à la date1

if ($date2 > $date1) { // comparaison des deux dates
    echo "La date 2 est postérieure à la date 1.";
}
// output : La date 2 est postérieure à la date 1.
```

# Plongée en profondeur dans la comparaison de dates en PHP

En PHP, les dates sont généralement représentées sous forme de timestamp, qui correspond au nombre de secondes écoulées depuis le 1er janvier 1970 à minuit UTC. Cela permet une meilleure précision dans les calculs de dates, notamment lorsque l'on doit prendre en compte les années bissextiles.

En utilisant les fonctions "date" et "strtotime", vous pouvez également formater et modifier les dates selon vos besoins. Par exemple, pour obtenir la date du lendemain à partir d'une date donnée :

```PHP
$date = strtotime("today"); // aujourd'hui
$tomorrow = strtotime("+1 day", $date); // ajout d'un jour à la date
echo date("Y-m-d", $tomorrow); // formatage de la date au format YYYY-mm-dd
// output : 2020-07-19
```

Il est également important de prendre en compte les fuseaux horaires lors de la comparaison de dates, surtout si vous travaillez sur un projet international.

# Voir aussi

- [Fonctions prédéfinies pour les dates en PHP](https://www.php.net/manual/fr/ref.datetime.php)
- [Documentation sur les fonctions de manipulation des dates en PHP](https://www.php.net/manual/fr/datetime.formats.relative.php)
- [Article sur la gestion des fuseaux horaires en PHP](https://www.php.net/manual/fr/datetime.settimezone.php)