---
title:    "PHP: Comparaison de deux dates"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates est un concept essentiel en programmation PHP. Elle permet de vérifier si une date est antérieure, postérieure ou égale à une autre et peut être utile pour de nombreuses applications, notamment pour les sites web ou les applications utilisant des données temporelles.

## Comment faire

Voici un exemple de code qui compare deux dates en utilisant la fonction `strtotime` pour les convertir en timestamp et la fonction `date` pour afficher le résultat :

```PHP
$premiere_date = "10-12-2021";
$deuxieme_date = "15-12-2021";
if (strtotime($premiere_date) > strtotime($deuxieme_date)) {
    echo date("d-m-Y", strtotime($premiere_date)) . " est postérieure à " . date("d-m-Y", strtotime($deuxieme_date));
} elseif (strtotime($premiere_date) < strtotime($deuxieme_date)) {
    echo date("d-m-Y", strtotime($premiere_date)) . " est antérieure à " . date("d-m-Y", strtotime($deuxieme_date));
} else {
    echo date("d-m-Y", strtotime($premiere_date)) . " est égale à " . date("d-m-Y", strtotime($deuxieme_date));
}
```

Cela produira la sortie suivante :

```
10-12-2021 est antérieure à 15-12-2021
```

Les opérateurs de comparaison `<`, `>` et `==` peuvent également être utilisés dans une condition pour comparer directement les dates sans les convertir en timestamp.

## Plongée en profondeur

Comme mentionné précédemment, la fonction `strtotime` est couramment utilisée pour convertir les dates en timestamp. Cependant, il est important de noter que cette fonction est sensible au format de la date. Par exemple, `strtotime("01-02-2021")` et `strtotime("2021-01-02")` donneront des résultats différents car ils sont en format jour-mois-année et année-mois-jour respectivement.

De plus, la fonction `strtotime` peut également être utilisée avec des chaînes telles que "next week" ou "last day of last month" pour obtenir les timestamps correspondants. Cette fonction peut être très pratique pour effectuer des opérations complexes sur les dates.

## Voir aussi

- [Documentation officielle de PHP sur la comparaison de dates](https://www.php.net/manual/fr/datetime.diff.php)
- [Tutoriel pour comparer les dates en PHP](https://www.php.net/manual/fr/datetime.diff.php)
- [Autre méthode pour comparer les dates en utilisant la classe DateTime](https://www.php.net/manual/fr/class.datetime.php)