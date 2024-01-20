---
title:                "Manipulation de JSON"
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
(Quoi et Pourquoi ?)

Travailler avec JSON, c'est manipuler des données structurées comme un texte. Les programmeurs utilisent JSON pour échanger facilement des informations entre différents langages de programmation et services.

## How to:
(Comment faire :)

Pour travailler avec JSON en Elixir, on utilise la librairie `Jason`. D'abord, ajoutez `Jason` à vos dépendances dans `mix.exs` :

```elixir
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end
```

Ensuite, parsez et générez du JSON avec :

```elixir
# Parsez du JSON
json_string = "{\"key\": \"value\"}"
{:ok, parsed} = Jason.decode(json_string)
# parsed est maintenant %{“key” => “value”}

# Générez du JSON
map = %{"key" => "value"}
{:ok, json} = Jason.encode(map)
# json est maintenant "{\"key\":\"value\"}"
```

Si vous exécutez ce code, vous obtenez un map pour le parsing, et une string pour la génération.
## Deep Dive:
(Exploration Détaillée :)

`Jason` est une bibliothèque JSON performante pour Elixir, prenant la relève d'options plus anciennes comme `Poison`. Elle se distingue par sa simplicité et sa conformité aux spécifications JSON. Par rapport à XML, l'alternative historique, JSON est moins verbeux et plus rapide à parser. De plus, il est devenu le format de choix dans les API REST et services web modernes.

## See Also:
(Voir aussi :)

- [Jason Repository on GitHub](https://github.com/michalmuskala/jason) - pour les instructions d'installation et la documentation complète.
- [JSON Specification (RFC 8259)](https://tools.ietf.org/html/rfc8259) - pour comprendre les règles et les standards de JSON.