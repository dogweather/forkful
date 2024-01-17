---
title:                "Travailler avec les fichiers csv"
html_title:           "Ruby: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Travailler avec les fichiers de valeurs séparées par des virgules, ou CSV, est un moyen courant pour les programmeurs de gérer et manipuler des données tabulaires. Les fichiers CSV sont faciles à créer, lire et écrire avec du code et peuvent être utilisés pour stocker des informations comme des feuilles de calcul ou des bases de données.

## Comment faire:

Voici quelques exemples de code en Ruby pour vous montrer comment utiliser les fichiers CSV:

```Ruby
require 'CSV'

# Lecture d'un fichier CSV
csv = CSV.read("mon_fichier.csv", headers: true, col_sep: ';')

# Écriture dans un fichier CSV
CSV.open("ma_nouvelle_feuille.csv", "wb") do |csv|
  csv << ["en-tête1", "en-tête2", "en-tête3"]
  csv << ["donnée1.1", "donnée1.2", "donnée1.3"]
  csv << ["donnée2.1", "donnée2.2", "donnée2.3"]
end
```

Voici ce à quoi pourrait ressembler votre sortie CSV:

| en-tête1 | en-tête2 | en-tête3 |
|----------|----------|----------|
| donnée1.1 | donnée1.2 | donnée1.3 |
| donnée2.1 | donnée2.2 | donnée2.3 |

## Plongée en profondeur:

Les fichiers CSV ont été créés dans les années 1970 pour faciliter l'échange de données entre différents systèmes informatiques. Ils sont largement utilisés aujourd'hui pour les importations et exportations de données, les migrations de base de données et les rapports.

Bien que les fichiers CSV soient un moyen populaire de stocker des données tabulaires, il existe d'autres formats tels que JSON, XML et YAML qui peuvent également être utilisés. Cependant, CSV reste le format préféré pour les données largement structurées et il est facile à manipuler avec du code.

Dans Ruby, la méthode ```CSV.read``` renvoie un tableau avec toutes les lignes du fichier CSV, tandis que la méthode ```CSV.open``` permet d'écrire dans un fichier CSV en lui passant un bloc.

## Voir aussi:

- [La documentation officielle de Ruby sur la gestion des fichiers CSV](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)
- [Une introduction aux fichiers CSV en Ruby](https://www.rubyguides.com/2018/10/csv-ruby/)
- [Un tutoriel vidéo sur la manipulation de fichiers CSV en Ruby](https://www.youtube.com/watch?v=dX4Hease1W8)