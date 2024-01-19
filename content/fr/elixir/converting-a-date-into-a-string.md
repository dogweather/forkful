---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce & Pourquoi?
Convertir une date en chaîne de caractères est le processus de transformation d'un objet date en une chaîne lisible. Les programmeurs le font pour faciliter l'affichage et le stockage des dates.

## Comment faire:
La bibliothèque standard Elixir `Date` peut être utilisée pour la conversion. Voici un exemple:

```Elixir
iex> date = ~D[2022-08-20]
~D[2022-08-20]
iex> date_string = to_string(date)
"2022-08-20"
```
Dans cet exemple, `~D[2022-08-20]` crée un objet `Date`. `to_string(date)` le convertit ensuite en chaîne.

## Approfondissement
Historiquement, différents formats de chaînes ont été utilisés pour représenter les dates, ce qui peut parfois créer de la confusion. Dans Elixir, la conversion de date en chaîne respecte la norme ISO 8601 pour éviter ce problème.

Il existe d'autres manières de faire une telle conversion, comme l'utilisation de la bibliothèque Timex. Cependant, le module `Date` intégré est souvent suffisant pour les besoins courants.

Lors de l'implémentation, la fonction `to_string` convertit la date en chaîne en formatant chaque attribut de la date (jour, mois et année) en une chaîne, puis en les combinant.

## Voir aussi
Pour plus d'informations, consultez les ressources suivantes :
- La documentation officielle d'Elixir sur le module `Date`: https://hexdocs.pm/elixir/Date.html
- Un guide pour manipuler les dates et les temps dans Elixir : https://pragmaticstudio.com/tutorials/working-with-dates-and-times-in-elixir
- La bibliothèque Timex pour les opérations de date et de temps plus avancées : https://hexdocs.pm/timex/readme.html.