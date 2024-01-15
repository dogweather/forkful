---
title:                "Travailler avec des fichiers CSV"
html_title:           "Ruby: Travailler avec des fichiers CSV"
simple_title:         "Travailler avec des fichiers CSV"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Travailler avec des fichiers CSV peut être très utile dans de nombreux cas, notamment lorsqu'il s'agit de stocker et de manipuler de grandes quantités de données. Avec Ruby, vous pouvez facilement lire, écrire et manipuler des fichiers CSV, ce qui en fait un langage très populaire pour travailler avec ce type de fichiers.

## Comment faire

Pour travailler avec des fichiers CSV en Ruby, vous devez tout d'abord importer la bibliothèque "csv". Voici un exemple de code pour lire un fichier CSV et afficher son contenu :

```Ruby
require 'csv'

CSV.foreach("fichier.csv") do |row|
    puts row
end
```

Ce code utilise la méthode `foreach` pour parcourir chaque ligne du fichier CSV et la méthode `puts` pour afficher cette ligne. Vous pouvez également utiliser la méthode `read` pour lire le contenu complet du fichier dans une variable et le manipuler comme bon vous semble.

Si vous souhaitez créer un nouveau fichier CSV à partir de données existantes, vous pouvez utiliser la méthode `open` pour ouvrir un nouveau fichier et lui ajouter des lignes à l'aide de la méthode `<<`. Voici un exemple de code pour créer un nouveau fichier CSV contenant des données de fruits et de quantités associées :

```Ruby
require 'csv'

CSV.open("nouveau_fichier.csv", "w") do |csv|
    csv << ["Fruit", "Quantité"]
    csv << ["Pomme", 5]
    csv << ["Banane", 10]
end
```

Cela créera un fichier CSV avec les en-têtes "Fruit" et "Quantité" et deux lignes de données. Bien sûr, vous pouvez ajouter autant de lignes que vous le souhaitez en utilisant la méthode `<<` autant de fois que nécessaire.

## Plongée en profondeur

Travailler avec des fichiers CSV peut être plus complexe que simplement lire ou écrire des données. Par exemple, vous pouvez avoir besoin de formater des données spécifiques ou de trier des colonnes en fonction de certains critères. Heureusement, Ruby propose de nombreuses méthodes pour vous aider à gérer ces tâches.

La méthode `sort_by` vous permet de trier les lignes d'un fichier CSV en utilisant une colonne spécifique comme clé de tri. Par exemple, si vous avez un fichier CSV contenant des données sur des employés et leur salaire, vous pouvez le trier par salaire décroissant en utilisant cette méthode. Vous pouvez également utiliser la méthode `find` pour rechercher des données spécifiques dans un fichier CSV en fonction de certains critères.

## Voir aussi

- La documentation officielle de Ruby sur la manipulation de fichiers CSV : https://ruby-doc.org/stdlib-2.6.3/libdoc/csv/rdoc/CSV.html
- Un tutoriel complet sur la manipulation de fichiers CSV en Ruby : https://www.sitepoint.com/guide-ruby-csv-library-part/
- Une bibliothèque externe pour faciliter la manipulation de fichiers CSV en Ruby : https://github.com/tilo/simplecsv