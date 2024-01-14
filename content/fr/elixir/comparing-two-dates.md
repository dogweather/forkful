---
title:                "Elixir: Comparer deux dates"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de dates est une tâche courante en programmation et peut être utile pour de nombreuses raisons, telles que vérifier si une date est antérieure ou postérieure à une autre, calculer la durée entre deux dates, ou encore trier des données chronologiquement. C'est une compétence importante pour tout développeur Elixir et dans cet article, nous allons apprendre comment comparer deux dates en utilisant le langage Elixir.

## Comment faire

Pour comparer deux dates en Elixir, nous allons utiliser la fonction `NaïveDateTime.compare/2`. Cette fonction prend deux paramètres de type `NaïveDateTime` et renvoie un entier négatif si la première date est antérieure à la seconde, zéro si les deux dates sont égales, et positif si la première date est postérieure à la seconde.

Voici un exemple de code : 

```Elixir
defmodule DateComparer do
  def compare_dates(date1, date2) do
    diff = NaiveDateTime.compare(date1, date2)
    
    case diff do
      -1 -> "#{date1} est antérieure à #{date2}"
      0 -> "Les deux dates sont identiques"
      1 -> "#{date1} est postérieure à #{date2}"
    end
  end
end

date1 = NaiveDateTime.new(2021, 3, 15)
date2 = NaiveDateTime.new(2020, 8, 21)

DateComparer.compare_dates(date1, date2)
```

Et voici le résultat :

> 2021-03-15 est postérieure à 2020-08-21

Nous pouvons également utiliser la fonction `DateTime.compare/2` pour comparer des dates avec un fuseau horaire. Cette fonction fonctionne de la même manière que `NaiveDateTime.compare/2`, mais prend deux paramètres de type `DateTime` et renvoie une valeur de `:lt` (moins que), `:eq` (égal) ou `:gt` (plus que).

## Analyse approfondie

Il est important de noter que la comparaison de dates en Elixir se fait en fonction de leur ordre chronologique et non de leur valeur numérique. Par exemple, la date `2021-03-15` est considérée comme postérieure à la date `2020-08-21` car elle vient chronologiquement après.

De plus, lors de la comparaison de dates avec un fuseau horaire, il est important de s'assurer que les deux dates ont le même fuseau horaire. Sinon, la comparaison peut renvoyer des résultats inattendus.

## Voir aussi

- [Documentation officielle d'Elixir sur les dates et heures](https://hexdocs.pm/elixir/DateTime.html)
- [Article sur la manipulation des dates en Elixir](https://medium.com/@codelabs/working-with-dates-and-times-in-elixir-49c71a11156b) (en anglais)