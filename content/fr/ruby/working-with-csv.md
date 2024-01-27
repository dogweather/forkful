---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Le CSV, "Comma-separated values", c’est simple: des données séparées par des virgules. Les devs l'adorent pour sa simplicité, son universalité avec les tableurs et sa facilité d’import/export dans des bases de données.

## How to:

Installation de la gem CSV:

```Ruby
gem install csv
```

Lire un fichier CSV:

```Ruby
require 'csv'

CSV.foreach("chemin/vers/fichier.csv", headers: true) do |row|
  puts row["Nom de la Colonne"]
end
```

Écrire dans un fichier CSV:

```Ruby
require 'csv'

CSV.open("chemin/vers/nouveau_fichier.csv", "w", write_headers: true, headers: ["Colonne 1", "Colonne 2"]) do |csv|
  csv << ["Valeur 1", "Valeur 2"]
end
```

## Deep Dive

Le format CSV date des années 70. Bon, il n’est pas parfait pour des données complexes, mais il reste un choix solide pour l'échange de données simples.

Il existe d’autres formats comme JSON ou XML. Ils sont mieux pour des structures compliquées, mais le CSV gagne en légèreté et facilité.

Ruby utilise sa librairie CSV standard. Elle est bien intégrée et efficace, mais si vous voulez plus de performance, Smarter_CSV est une alternative populaire.

## See Also

Documentation officielle de Ruby sur CSV: [Ruby CSV](https://ruby-doc.org/stdlib-2.6/libdoc/csv/rdoc/CSV.html)

Gem Smarter_CSV pour un CSV handling plus rapide: [Smarter_CSV](https://github.com/tilo/smarter_csv)

Tutoriel pour apprendre à manipuler le CSV avec Ruby: [Ruby CSV Tutorial](https://www.callicoder.com/ruby-csv-read-write/)
