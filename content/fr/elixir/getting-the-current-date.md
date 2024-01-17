---
title:                "Obtenir la date actuelle"
html_title:           "Elixir: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi faire ?
Obtenir la date courante signifie obtenir la date et l'heure actuelles sur votre système ou serveur. Cela peut être utile pour une variété de raisons, telles que suivre l'heure d'exécution d'un programme, synchroniser des données, ou simplement afficher l'heure actuelle dans une application.

## Comment faire :
Pour obtenir la date courante en Elixir, vous pouvez utiliser la fonction `DateTime.utc_now()` ou `NaiveDateTime.utc_now()`. Voici un exemple qui imprime la date et l'heure actuelles :

```
DateTime.utc_now()
=> ~U[2021-09-28 11:10:32.305479Z]
```

## Plongée en profondeur :
La fonction `DateTime.utc_now()` renvoie un objet de type `DateTime` qui représente la date et l'heure en temps universel. D'autre part, la fonction `NaiveDateTime.utc_now()` renvoie un objet de type `NaiveDateTime` qui n'a pas de fuseau horaire et est utilisée pour effectuer des calculs de temps sans avoir à se soucier des fuseaux horaires.

Si vous préférez afficher la date et l'heure dans un format différent, vous pouvez utiliser la fonction `strftime` pour formater la date et l'heure actuelles selon vos besoins.

## Voir aussi :
Pour en savoir plus sur la manipulation des dates et heures en Elixir, vous pouvez consulter la documentation officielle de la fonction `DateTime` et la fonction `NaiveDateTime`.

Documentation officielle DateTime : https://hexdocs.pm/elixir/DateTime.html
Documentation officielle NaiveDateTime : https://hexdocs.pm/elixir/NaiveDateTime.html