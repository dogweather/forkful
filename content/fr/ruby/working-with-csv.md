---
title:                "Ruby: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Le format CSV (Comma-Separated Values) est couramment utilisé pour stocker des données tabulaires dans un fichier texte. En travaillant avec des données en bulle, que ce soit pour une analyse de données ou pour l'intégration avec d'autres logiciels, il peut être utile d'avoir une certaine connaissance de la façon de travailler avec des fichiers CSV en Ruby.

## Comment faire

La bibliothèque standard Ruby CSV fournit des méthodes pour lire et écrire des fichiers CSV. Voici un exemple de code pour lire un fichier CSV et afficher son contenu :

```Ruby
require 'csv'

CSV.foreach('donnees.csv') do |row|
  puts row.inspect
end
```

Cela va afficher chaque ligne du fichier CSV dans un tableau. Si vous souhaitez afficher les données d'une colonne spécifique, vous pouvez utiliser l'indice de colonne pour accéder à cette valeur. Par exemple :

```Ruby
require 'csv'

CSV.foreach('donnees.csv') do |row|
  puts row[0] # affiche la valeur de la première colonne
  puts row[1] # affiche la valeur de la deuxième colonne
end
```

Pour écrire dans un fichier CSV, vous pouvez utiliser la méthode `CSV.open` :

```Ruby
require 'csv'

CSV.open('nouveaux_donnees.csv', 'w') do |csv|
  csv << ['Nom', 'Prénom', 'Âge']
  csv << ['Dupont', 'Jean', '35']
  csv << ['Martin', 'Marie', '28']
end
```

Cela va créer un nouveau fichier CSV avec les données fournies. Vous pouvez également utiliser d'autres options telles que `row_sep` pour spécifier le séparateur de ligne et `col_sep` pour spécifier le séparateur de colonne.

## Plongée profonde

Lorsque vous travaillez avec des fichiers CSV en Ruby, il est important de prendre en compte certaines caractéristiques telles que le support d'Unicode, la gestion des virgules et guillemets dans les données, et la possibilité de spécifier un délimiteur personnalisé. La bibliothèque standard CSV offre également des méthodes pour gérer ces aspects lors de la lecture ou de l'écriture de fichiers CSV.

De plus, il peut être utile de savoir comment traiter les erreurs lors de la lecture ou de l'écriture de fichiers CSV, ainsi que de comprendre comment fonctionne le parsing avec la méthode `CSV.parse`. En explorant ces sujets plus en profondeur, vous serez en mesure de gérer des fichiers CSV de manière plus avancée dans vos projets Ruby.

## Voir aussi

- [Documentation officielle pour la bibliothèque standard CSV en Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/csv/rdoc/CSV.html)
- [Article détaillant les bonnes pratiques pour travailler avec des fichiers CSV en Ruby](https://www.sitepoint.com/guide-ruby-csv-library-part/)
- [Gemme FasterCSV pour manipulation de données CSV plus rapide](https://github.com/JEG2/faster_csv)