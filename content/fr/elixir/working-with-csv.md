---
title:                "Manipulation des fichiers CSV"
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

CSV, c'est du texte simple, souvent pour des tableaux. Les dev' utilisent CSV pour échanger des données facilement entre systèmes.

## How to:

Pour lire un CSV :

```elixir
# Installez la dépendance {:nimble_csv, "~> 1.0"}
defmodule MyCSV do
  use NimbleCSV, separator: ",", escape: "\""
end

# Lire un fichier CSV
{:ok, content} = File.read("data.csv")
rows = MyCSV.parse_string(content)

# Affiche les lignes du CSV
IO.inspect(rows)
```

Pour écrire dans un CSV :

```elixir
# Utiliser MyCSV défini précédemment
data = [["nom", "age"], ["Jean", "34"], ["Marie", "28"]]

# Transformer les données en chaîne CSV
csv_content = MyCSV.dump_to_iodata(data)

# Écrire dans un fichier
File.write("output.csv", csv_content)

# Sortie - le contenu de output.csv :
# nom,age
# Jean,34
# Marie,28
```

## Deep Dive

CSV est vieux, simple, mais n'est pas standardisé : attention aux séparateurs et guillemets. Alternatives ? JSON pour structure, SQLite pour requêtes. CSV en Elixir = facilité avec NimbleCSV pour performer l'équilibre entre facilité d'emploi et flexibilité.

## See Also

- [RFC 4180, la "norme" CSV](https://tools.ietf.org/html/rfc4180)
- [Elixir School pour apprendre Elixir](https://elixirschool.com/fr/)
