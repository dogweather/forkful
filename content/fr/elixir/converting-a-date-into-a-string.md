---
title:                "Elixir: Transformer une date en chaîne de caractères."
simple_title:         "Transformer une date en chaîne de caractères."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une date en chaîne de caractères est un concept clé en programmation Elixir. Cela permet de manipuler facilement et d'afficher des dates dans un format compréhensible pour les utilisateurs. Dans cet article, nous allons découvrir pourquoi et comment convertir une date en chaîne de caractères en utilisant Elixir.

## Comment faire

Il existe plusieurs façons de convertir une date en chaîne de caractères en Elixir. Voici un exemple de code utilisant la fonction `date.to_string/2` :

```Elixir
date = ~D[2021-05-01] # Définit une date au format ISO
formatted_date = date.to_string({:short, :numeric}) # Convertit la date en chaîne de caractères
IO.puts formatted_date
```

Le code ci-dessus va afficher la date sous la forme `05/01/2021`. Vous pouvez également personnaliser le format de la date en utilisant les options `:short`, `:medium`, `:long` ou `:iso` ainsi que les options `:numeric` ou `:verbose` pour afficher des nombres ou des mots.

Vous pouvez aussi utiliser la fonction `Date.to_iso8601/1` pour convertir une date en chaîne de caractères conforme au standard ISO 8601 :

```Elixir
date = ~D[2021-05-01]
ISO_date = Date.to_iso8601(date)
IO.puts ISO_date
```

Le code ci-dessus va afficher la date sous la forme `2021-05-01`.

## Plongée en profondeur

En plus des fonctions mentionnées ci-dessus, Elixir offre également la possibilité de manipuler des dates en utilisant le module `Calendar`. Ce module fournit un ensemble de fonctions puissantes pour gérer les dates, y compris la conversion en chaîne de caractères.

Par exemple, vous pouvez utiliser la fonction `~D` pour convertir une date au format ISO en utilisant les paramètres de votre choix :

```Elixir
date = ~D[2021-05-01]
formatted_date = "~D/%d-%m-%Y" |> String.replace(~r/\//, "-") |> Calendar.strftime(date)
IO.puts formatted_date
```

Le code ci-dessus va afficher la date sous la forme `01-05-2021`. Vous pouvez également utiliser la fonction `~U` pour convertir une date avec une heure spécifique :

```Elixir
date = ~U[2021-05-01 15:30:00]
formatted_date = "~U/%Y-%m-%dT%H:%M:%S%z" |> Calendar.strftime(date)
IO.puts formatted_date
```

Le code ci-dessus va afficher la date sous la forme `2021-05-01T15:30:00+00:00`.

## Voir aussi

- [Documentation officielle Elixir pour le module Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Article "7 astuces pour travailler avec les dates en Elixir"](https://medium.com/@leovillani/7-astuces-pour-travailler-avec-les-dates-en-elixir-444b786174bd)
- [Blog de la communauté Elixir en français](https://elixir-fr.com)