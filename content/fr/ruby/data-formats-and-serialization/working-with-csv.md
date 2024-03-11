---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:05.010493-07:00
description: "Travailler avec des fichiers CSV en Ruby offre une approche simple pour\
  \ g\xE9rer les donn\xE9es tabulaires. Les programmeurs se livrent souvent \xE0 cette\
  \ pratique\u2026"
lastmod: '2024-03-11T00:14:32.315447-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV en Ruby offre une approche simple pour\
  \ g\xE9rer les donn\xE9es tabulaires. Les programmeurs se livrent souvent \xE0 cette\
  \ pratique\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV en Ruby offre une approche simple pour gérer les données tabulaires. Les programmeurs se livrent souvent à cette pratique pour le parsing de données, l'extraction, la transformation et le stockage, ce qui en fait une compétence essentielle pour les tâches impliquant la manipulation ou l'analyse de données.

## Comment faire :

Ruby inclut par défaut la bibliothèque CSV, ce qui simplifie la lecture et l'écriture de fichiers CSV. Voici comment vous pouvez tirer parti de cela pour des tâches courantes :

### Lire un fichier CSV
Pour lire depuis un fichier CSV, vous avez d'abord besoin de la bibliothèque CSV. Ensuite, vous pouvez itérer sur les lignes ou les lire dans un tableau.

```ruby
require 'csv'

# Lire chaque ligne comme un tableau
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# La sortie pour chaque ligne pourrait ressembler à ceci : ["data1", "data2", "data3"]
```

### Écrire dans un CSV
Écrire dans un fichier CSV est également simple. Vous pouvez ajouter à un fichier existant ou créer un nouveau fichier pour écrire.

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["entête1", "entête2", "entête3"]
  csv << ["valeur1", "valeur2", "valeur3"]
end

# Ceci crée ou écrase 'output.csv' avec les entêtes et valeurs spécifiées.
```

### Analyser une chaîne CSV
Parfois, vous devez analyser les données CSV directement à partir d'une chaîne. Voici comment faire :

```ruby
require 'csv'

data = "nom,âge,ville\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, with_headers: true)

csv.each do |row|
  puts "#{row['nom']} - #{row['âge']} - #{row['ville']}"
end

# Sortie attendue :
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### Utiliser SmarterCSV
Pour des tâches CSV plus complexes, le gem `SmarterCSV` peut être un outil précieux. D'abord, installez le gem :

```shell
gem install smarter_csv
```

Ensuite, vous pouvez l'utiliser pour traiter de grands fichiers ou effectuer un parsing et une manipulation plus sophistiqués :

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# Ceci lira 'large_data.csv' et sortira chaque ligne comme un hash basé sur les entêtes.
```

En résumé, la bibliothèque CSV intégrée de Ruby, ainsi que des gems tiers comme `SmarterCSV`, fournissent un soutien robuste pour la gestion des données CSV, permettant des tâches de traitement et de manipulation de données efficaces.
