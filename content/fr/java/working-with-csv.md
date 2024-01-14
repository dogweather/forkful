---
title:                "Java: Travailler avec les fichiers CSV"
simple_title:         "Travailler avec les fichiers CSV"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de la programmation, vous vous demandez peut-être ce qu'est un CSV. En bref, c'est un format de fichier couramment utilisé pour stocker des données structurées. En travaillant avec des fichiers CSV, vous serez en mesure d'organiser et de manipuler des données de manière efficace. 

## Comment faire

Pour commencer à travailler avec des fichiers CSV en Java, vous devrez d'abord importer la bibliothèque Java CSV. Ensuite, vous pouvez utiliser la classe ```CSVReader``` pour lire les données à partir d'un fichier CSV et la classe ```CSVWriter``` pour écrire des données dans un fichier CSV. Voici un exemple de code pour lire un fichier CSV et imprimer son contenu :

```
CSVReader reader = new CSVReader(new FileReader("mon_fichier.csv"));
String[] ligne;
while ((ligne = reader.readNext()) != null) {
    for (String cellule : ligne) {
        System.out.print(cellule + " | ");
    }
    System.out.println();
}
```

En utilisant le code ci-dessus, nous pouvons lire chaque ligne du fichier CSV et imprimer le contenu de chaque cellule séparé par des barres verticales. L'exemple de code ci-dessus utilise la bibliothèque OpenCSV, mais vous pouvez également utiliser d'autres bibliothèques telles que Apache Commons CSV ou Super CSV pour travailler avec des fichiers CSV en Java.

Maintenant, voyons un exemple de code pour écrire des données dans un fichier CSV :

```
CSVWriter writer = new CSVWriter(new FileWriter("nouveau_fichier.csv"));
String[] entetes = {"Nom", "Prénom", "Âge"};
writer.writeNext(entetes);
String[] ligne1 = {"Dupont", "Marc", "25"};
writer.writeNext(ligne1);
String[] ligne2 = {"Martin", "Sophie", "30"};
writer.writeNext(ligne2);
writer.close();
```

Dans cet exemple, nous avons d'abord créé un en-tête pour notre fichier CSV, puis nous avons écrit deux lignes de données. Le fichier CSV résultant aura la structure suivante :

| Nom | Prénom | Âge |
|-----|--------|------|
| Dupont | Marc | 25 |
| Martin | Sophie | 30 |

## Plongée en profondeur

Bien que travailler avec des fichiers CSV puisse sembler simple au premier abord, il y a quelques points à prendre en compte pour éviter les erreurs. Tout d'abord, vous devez vous assurer que vos données sont correctement formatées pour éviter les problèmes lors de la lecture ou de l'écriture. Vous devez également prendre en compte les différents encodages de caractères qui peuvent être utilisés dans les fichiers CSV.

Il est également important de comprendre comment gérer les erreurs lors de la lecture ou de l'écriture de fichiers CSV. Par exemple, si un fichier CSV a des lignes vides ou des colonnes manquantes, vous devez être en mesure de les détecter et de les traiter correctement.

Enfin, il est crucial de savoir comment manipuler les données lues à partir d'un fichier CSV. Vous pouvez avoir besoin de convertir des données en différents types ou de les traiter avant de les utiliser pour votre application.

## Voir aussi

- [Documentation Java sur les fichiers CSV](https://docs.oracle.com/javase/8/docs/api/java/csv/package-summary.html)
- [Bibliothèque OpenCSV](https://sourceforge.net/projects/opencsv/)
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/)
- [Super CSV](https://super-csv.github.io/super-csv/index.html)