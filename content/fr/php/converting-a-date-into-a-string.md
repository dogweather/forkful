---
title:                "PHP: Convertir une date en une chaîne de caractères"
simple_title:         "Convertir une date en une chaîne de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le développement web, il est parfois nécessaire de travailler avec des dates. Cependant, il est souvent plus pratique d'avoir ces dates sous forme de chaînes de caractères plutôt que sous forme de données brutes. Cela peut faciliter l'affichage ou la manipulation des dates. Dans cet article, nous allons voir comment convertir une date en une chaîne de caractères en utilisant PHP.

## Comment faire

Pour convertir une date en chaîne de caractères en PHP, il existe une fonction intégrée appelée `date()`. Cette fonction prend en paramètre un format de date et retourne la date actuelle dans ce format. Voici un exemple de code qui montre comment utiliser cette fonction :

```PHP 
$date = date('d/m/Y'); // retourne la date actuelle sous la forme "jour/mois/année"
echo $date; // affiche "18/09/2021"
```

Comme vous pouvez le voir, en utilisant `date()`, nous pouvons facilement transformer une date en une chaîne de caractères. Mais qu'en est-il si nous voulons afficher une date spécifique ? Par exemple, la date de notre anniversaire qui est toujours le 10 avril ? Pour cela, nous pouvons utiliser la fonction `mktime()` qui prend en paramètres l'heure, le mois, le jour et l'année de la date que nous voulons obtenir. Voici un exemple de code pour obtenir la date de notre prochain anniversaire :

```PHP 
$date = mktime(0, 0, 0, 4, 10); // heure, minute, seconde, mois, jour;
echo date('d/m/Y', $date); // affiche "10/04/2022"
```

Nous pouvons également utiliser `strtotime()`, qui prend une chaîne de caractères représentant une date et la convertit en nombre de secondes écoulées depuis le 1er janvier 1970. Voici un exemple de code utilisant `strtotime()` :

```PHP 
$date = strtotime('next monday'); // représente lundi prochain
echo date('d/m/Y', $date); // affiche la date du prochain lundi
```

## Deep Dive

Maintenant que nous avons vu comment convertir une date en chaîne de caractères, intéressons-nous aux différents formats de date que nous pouvons utiliser. Voici quelques-uns des formats les plus courants :

- `d` représente le jour du mois (ex : 18)
- `m` représente le mois (ex : 09 pour septembre)
- `Y` représente l'année sur quatre chiffres (ex : 2021)
- `l` représente le jour de la semaine (ex : samedi)
- `F` représente le mois complet (ex : septembre)

Il existe également des caractères spéciaux qui permettent d'ajouter des séparateurs, des espaces, etc. Par exemple, `d/m/Y` affichera la date sous la forme "jour/mois/année" en séparant chaque élément par des barres obliques.

Il est également possible de personnaliser complètement le format de la date en utilisant `date()` avec la fonction `mktime()`. En utilisant `date()` avec le paramètre "format", nous pouvons spécifier le format souhaité et utiliser les caractères spéciaux pour obtenir le résultat désiré.

## Voir aussi

Pour en savoir plus sur la manipulation des dates en PHP, voici quelques liens utiles :

- [Documentation officielle de la fonction date()](https://www.php.net/manual/fr/function.date.php)
- [Utilisation de la fonction strtotime()](https://www.php.net/manual/fr/function.strtotime.php)
- [Conversion de formats de date avec mktime()](https://www.php.net/manual/fr/function.mktime.php)

Maintenant que vous savez comment convertir une date en chaîne de caractères en utilisant PHP, vous pouvez l'appliquer dans vos projets web pour un affichage ou une manipulation plus pratique des dates. N'hésitez pas à expérimenter avec les différents formats et fonctions pour trouver celui qui convient le mieux à vos besoins. À bientôt pour un prochain article sur la programmation en PHP !∏