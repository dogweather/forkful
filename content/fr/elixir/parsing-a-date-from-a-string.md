---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi ?
Parser une date à partir d'une chaîne signifie extraire des informations de date compréhensibles à partir d'un format texte. Les programmeurs font cela pour manipuler et afficher les informations de date de manière plus utile et formatée.

## Comment faire :
Elixir offre une bibliothèque appelée `DateTime`, qui facilite le traitement des dates. Utilisez `DateTime.from_iso8601` pour parser une date à partir d'une chaîne.

```elixir
{:ok, date} = DateTime.from_iso8601("2022-03-15T15:30:00Z")
IO.inspect(date)
```
Exécutez ce code, et vous verrez une structure de date Elixir, comme ci-dessous :
```elixir
#DateTime<2022-03-15T15:30:00Z>
```

## Plongée en profondeur
1. *Contexte historique* : Le besoin de parser des dates à partir de chaînes est aussi ancien que l'informatique elle-même. C'est un moyen courant de traiter les données temporelles, dû à la facilité de lecture et de stockage des chaînes.

2. *Alternatives* : Pour des formats de date non conventionnels, vous pouvez créer une fonction personnalisée, ou utiliser des bibliothèques externes comme `Timex`.

```elixir
def parse_custom_date(str) do
  [d, m, y] = String.split(str, "/") |> Enum.map(&String.to_integer/1)
  {:ok, date} = Date.new(y, m, d)
  date
end
```
3. *Détails de mise en œuvre* : Elixir utilise une approche basée sur le pattern-matching pour le parsing. Cela en fait une tâche relativement simple et directe comparée à d'autres langages qui peuvent nécessiter des regex ou des conversions complexes.

## Voir aussi
1. Le module [Date](https://hexdocs.pm/elixir/Date.html) de la documentation officielle d'Elixir.
2. La bibliothèque [Timex](https://hexdocs.pm/timex/readme.html) pour des fonctionnalités de date et heure plus avancées.
3. Article : [Elixir School - Working with date and time in Elixir](https://elixirschool.com/en/lessons/basics/date_time/).