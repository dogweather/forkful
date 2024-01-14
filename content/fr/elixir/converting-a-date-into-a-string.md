---
title:    "Elixir: Conversion d'une date en chaîne de caractères"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de convertir une date en chaîne de caractères dans nos programmes Elixir. Cela peut être utile pour afficher la date dans un format spécifique ou pour l'utiliser dans une requête HTTP. Heureusement, Elixir rend cette tâche assez simple et nous allons découvrir comment le faire dans cet article.

## Comment faire

La conversion d'une date en chaîne de caractères en Elixir se fait grâce à la fonction `DateTime.to_iso8601/1`. Celle-ci prend en paramètre une date au format Elixir et retourne une chaîne de caractères représentant cette date au format ISO8601.

Voici un exemple de code utilisant cette fonction:

```Elixir
date = ~D[2021-05-15]
DateTime.to_iso8601(date)

# Résultat: "2021-05-15T00:00:00Z"
```

Comme vous pouvez le voir, la date a été convertie en une chaîne de caractères avec le format "AAAA-MM-JJTHH:MM:SSZ". Ceci est le format standard pour les dates en Elixir.

Mais que faire si vous souhaitez un format différent? Pas de soucis, Elixir offre également la fonction `DateTime.to_string/3` qui permet de spécifier un format personnalisé pour la date.

```Elixir
date = ~D[2021-05-15]
DateTime.to_string(date, "{0}-{1}-{2}")

# Résultat: "2021-05-15"
```

Dans cet exemple, nous avons spécifié un format avec trois paramètres représentant l'année, le mois et le jour. La fonction `DateTime.to_string/3` utilise ces paramètres pour construire la chaîne de caractères finale.

## Plongée en profondeur

Pour mieux comprendre comment Elixir convertit une date en chaîne de caractères, il est important de savoir comment les dates sont représentées en Elixir.

Une date en Elixir est en fait un module `DateTime` qui contient plusieurs fonctions pour manipuler et afficher des dates. Parmi celles-ci, nous avons `DateTime.to_iso8601/1` et `DateTime.to_string/3` que nous avons vues précédemment.

Il est également important de noter que par défaut, Elixir utilise le fuseau horaire UTC pour toutes les dates. Cependant, vous pouvez spécifier un fuseau horaire différent en utilisant la fonction `DateTime.shift_zone/2`.

## Voir aussi

- La documentation officielle sur les dates en Elixir: <https://hexdocs.pm/elixir/DateTime.html>
- Un article sur la manipulation de dates en Elixir: <https://medium.com/@bogdanvlviv/handling-dates-and-times-in-elixir-9e77a51b6397>
- Une liste de bibliothèques pour travailler avec les dates en Elixir: <https://github.com/avelino/awesome-go#datetime>