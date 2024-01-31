---
title:                "Travailler avec YAML"
date:                  2024-01-19
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, c’est quoi ? Une langue de sérialisation de données facile à lire par les humains. Pourquoi les programmeurs l’utilisent ? Pour configurer des projets et stocker des structures de données sans tracas.

## How to:
```elixir
# Ajout de la dépendance dans mix.exs
defp deps do
  [
    {:yamerl, "~> 0.8.0"}
  ]
end

# Utilisation dans le code

# Pour lire YAML
{:ok, yaml} = File.read("config.yaml")
{:ok, result} = :yamerl_constr.string(yaml)
IO.inspect(result)

# Pour écrire YAML, on peut utiliser une autre librairie, comme `yex`
# Assurer qu'yex est ajouté dans deps
my_data = %{"hello" => "world"}
File.write!("output.yaml", Yex.encode!(my_data))
```

## Deep Dive
YAML a été créé en 2001, principalement par Clark Evans, aussi connu pour remplacer les fichiers de configuration XML moins lisibles. Contrairement au JSON, YAML supporte les commentaires et se prête mieux aux configurations manuelles. Elixir, n'a pas de support YAML intégré, mais grâce à la communauté, on utilise `yamerl` ou `yex`. `yamerl` est écrit en Erlang, ça joue bien avec Elixir pour la lecture de YAML, tandis que `yex` prend le relais pour l'écriture.

## See Also
- [YAML officiel](https://yaml.org)
- [HexDocs pour yamerl](https://hexdocs.pm/yamerl)
