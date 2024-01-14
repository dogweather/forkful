---
title:                "Elixir: Obtenir la date actuelle"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date et l'heure actuelles est une tâche courante dans la programmation Elixir. Cela peut être utile pour afficher des horaires, planifier des tâches ou simplement pour avoir une notion du temps dans vos applications. Dans cet article, nous allons explorer différentes façons de récupérer la date et l'heure actuelles en utilisant Elixir.

## Comment faire

La méthode la plus simple pour obtenir la date actuelle est d'utiliser la fonction `DateTime.utc_now/0` qui renvoie un objet `DateTime` représentant l'heure et la date actuelles en temps universel coordonné (UTC). Voici un exemple de code pour utiliser cette fonction :

```Elixir
date_time = DateTime.utc_now()
IO.puts "Il est actuellement : #{date_time}"
```

Lorsque vous exécutez ce code, vous verrez quelque chose comme ceci :

```
Il est actuellement : 2021-04-05 17:00:00Z
```

Si vous voulez récupérer la date et l'heure dans votre fuseau horaire local, vous pouvez utiliser la fonction `DateTime.local_now/0` :

```Elixir
date_time = DateTime.local_now()
IO.puts "Il est actuellement : #{date_time}"
```

Enfin, si vous souhaitez simplement obtenir la date actuelle, vous pouvez utiliser la fonction `Date.utc_today/0` ou `Date.local_today/0` en fonction de votre besoin :

```Elixir
date = Date.utc_today()
IO.puts "Aujourd'hui, c'est : #{date}"
```

Les fonctions mentionnées ci-dessus sont toutes disponibles dans le module `DateTime` et `Date` de la bibliothèque standard Elixir.

## Profondeur d'analyse

Maintenant que nous savons comment obtenir la date et l'heure actuelles en Elixir, il est important de comprendre un peu mieux ce qui se cache sous ces fonctions. Les valeurs renvoyées par `DateTime.utc_now/0` et `DateTime.local_now/0` sont en fait des tuples contenant toutes les informations utiles sur la date et l'heure. Voici à quoi ressemble un tuple renvoyé par la fonction `DateTime.utc_now/0` :

```Elixir
{2021, 4, 5, {17, 0, 0}, 0}
```

Les trois premiers éléments correspondent respectivement à l'année, au mois et au jour. Le quatrième élément est un tuple contenant l'heure, les minutes et les secondes. Le dernier élément est l'offset par rapport à UTC.

De même, le tuple renvoyé par la fonction `Date.utc_today/0` ressemble à ceci :

```Elixir
{2021, 4, 5}
```

Les trois éléments correspondent à la même chose que pour la fonction `DateTime.utc_now/0`.

Maintenant que vous savez comment fonctionnent ces fonctions, vous pouvez également créer vos propres fonctions personnalisées pour récupérer la date et l'heure dans le format qui vous convient.

## Voir aussi

- [Documentation Elixir sur DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Documentation Elixir sur Date](https://hexdocs.pm/elixir/Date.html)
- [Article de blog sur la manipulation des dates en Elixir](https://m.alphasights.com/how-to-work-with-dates-times-and-timezones-in-elixir-39e3acc4d864)