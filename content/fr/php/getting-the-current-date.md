---
title:    "PHP: Obtenir la date actuelle"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

La date est un élément essentiel dans toute application ou site web en PHP. Elle permet d'afficher des informations temporelles précises, telles que la date et l'heure d'un événement, ou encore de gérer des tâches programmées. Apprendre à récupérer la date actuelle sera donc utile à toutes les personnes voulant développer en PHP.

## Comment faire

Pour récupérer la date actuelle en PHP, il existe plusieurs méthodes. La plus simple consiste à utiliser la fonction `date()`, en lui passant en paramètre le format souhaité, tel que `d/m/Y` pour afficher la date au format jour/mois/année. Voici un exemple de code pour récupérer et afficher la date dans une variable :

```PHP
$date = date('d/m/Y');
echo "Aujourd'hui, nous sommes le " . $date;
```

Cela affichera : "Aujourd'hui, nous sommes le 25/07/2020".

Il est également possible d'utiliser la classe `DateTime`, qui offre plus de fonctionnalités, comme la possibilité de manipuler la date et l'heure. Voici un exemple de code :

```PHP
$date = new DateTime();
echo $date->format('d/m/Y');
```

Cela affichera également la date actuelle, mais en utilisant un objet DateTime.

## Plongée en profondeur

La fonction `date()` utilise le timestamp Unix pour récupérer la date actuelle. Ce timestamp représente le nombre de secondes écoulées depuis le 1er janvier 1970 à minuit UTC. Il est important de comprendre cela pour pouvoir manipuler correctement les dates et heures en PHP.

De plus, la classe `DateTime` offre de nombreuses méthodes pour modifier ou comparer des dates, ce qui peut être très utile dans certaines situations. Il est recommandé de se familiariser avec ces méthodes pour exploiter au maximum cette classe.

## Voir aussi

- [Documentation PHP sur la fonction date](https://www.php.net/manual/fr/function.date.php)
- [Documentation PHP sur la classe DateTime](https://www.php.net/manual/fr/class.datetime.php)
- [Manipuler les dates et heures en PHP](https://www.pierre-giraud.com/php-mysql-apprendre-coder-cours/manipulation-date-heure-php/) (article en français)