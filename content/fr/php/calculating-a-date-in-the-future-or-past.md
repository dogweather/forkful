---
title:    "PHP: Calculer une date dans le futur ou le passé"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou le passé est une compétence importante pour tout programmeur PHP. Cela peut être utile lors de la création d'applications de planification ou de la gestion de données historiques.

## Comment faire

Pour calculer une date dans le futur ou le passé, vous pouvez utiliser la fonction `date()` en PHP. Cette fonction prend deux paramètres : le format de la date souhaitée et le timestamp de la date de référence.

Voici un exemple de code pour calculer la date 7 jours dans le futur :

```PHP
$date = date("Y-m-d", strtotime("+7 days"));
echo $date; // Output: 2021-10-04
```

Et voici un exemple pour calculer la date 1 mois dans le passé :

```PHP
$date = date("Y-m-d", strtotime("-1 month"));
echo $date; // Output: 2021-08-04
```

Vous pouvez également spécifier différents formats de date selon vos besoins, tels que `d-m-Y` pour avoir la date au format jour-mois-année.

## Plongée en profondeur

Lorsque vous utilisez la fonction `date()` pour calculer une date dans le futur ou le passé, il est important de comprendre comment fonctionnent les timestamps. En PHP, le timestamp est un entier qui représente le nombre de secondes écoulées depuis le 1er janvier 1970 à minuit.

En utilisant cette valeur comme point de référence, la fonction `strtotime()` peut calculer une date en ajoutant ou en soustrayant un nombre spécifié de secondes au timestamp actuel.

De plus, vous pouvez également spécifier une date de référence différente en utilisant la fonction `strtotime()` et en passant deuxième paramètre contenant un autre timestamp.

## Voir aussi

- [Documentation PHP sur la fonction date()](https://www.php.net/manual/fr/function.date.php)
- [Documentation PHP sur la fonction strtotime()](https://www.php.net/manual/fr/function.strtotime.php)