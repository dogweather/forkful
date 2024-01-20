---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:35:30.832587-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Parser une date à partir d'une chaîne de caractères, c'est convertir du texte en une structure de date exploitable par le programme. Les programmeurs utilisent cet outil pour faciliter le traitement et la manipulation des dates provenant de sources externes, comme des fichiers ou des entrées utilisateur.

## How to (Comment faire)
Avec Elixir, le module `DateTime` rend la tâche aisée. On va voir comment. 

```elixir
# On utilise la fonction from_iso8601 pour parser une date ISO8601.
{date, _} = DateTime.from_iso8601("2023-04-01T13:45:30Z")
IO.inspect(date)

# Besoin d'un format différent ? On utilise Timex (une librairie tiers).
{:ok, timex_date} = Timex.parse("01-04-2023 14:30", "{0D}-{0M}-{YYYY} {k}:{m}", :strftime)
IO.inspect(timex_date)
```

Sortie :
```
# DateTime.from_iso8601
#<DateTime(2023-04-01T13:45:30Z)>

# Timex.parse
#<DateTime(2023-04-01T14:30:00Z)>
```

## Deep Dive (Plongée profonde)
Historiquement, la gestion des dates en informatique est complexe. Des formats différents, fuseaux horaires et problèmes de localisation rendent le parsing délicat. Elixir offre `DateTime.from_iso8601` pour les formats standards. Pour plus de souplesse et de formats, on utilise souvent Timex, une librairie externe qui offre des fonctions puissantes de manipulation de dates.

Pourquoi ne pas utiliser la fonction `strptime` de la bibliothèque standard ? `DateTime.from_iso8601` est mieux intégré à l'écosystème Elixir et optimisé pour la performance et la précision. Timex, c'est le couteau suisse des dates pour les cas plus complexes.

À l'intérieur, faire le parsing d'une date implique des calculs sur les calendriers, la prise en compte des décalages horaires et des secondes intercalaires. Ces mécanismes sont abstraits par Elixir et Timex, simplifiant le travail des développeurs.

## See Also (Voir aussi)
- La documentation officielle de `DateTime` : https://hexdocs.pm/elixir/DateTime.html
- GitHub de la librairie Timex : https://github.com/bitwalker/timex
- ISO 8601 sur Wikipedia : https://fr.wikipedia.org/wiki/ISO_8601