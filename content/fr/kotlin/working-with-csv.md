---
title:                "Kotlin: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

Bonjour les programmeurs Kotlin ! Si vous avez déjà travaillé avec des données tabulaires dans vos projets, vous avez probablement rencontré le format CSV. CSV, ou Comma-Separated Values, est un moyen courant de stocker des données sous forme de tableaux dans des fichiers texte. Dans cet article, nous allons explorer pourquoi et comment travailler avec des fichiers CSV en utilisant Kotlin.

## Pourquoi

Les fichiers CSV sont un format de données simple et convivial qui est couramment utilisé pour stocker des données dans des applications web, des feuilles de calcul et des bases de données relationnelles. En travaillant avec des fichiers CSV, vous pouvez facilement importer et exporter des données dans différents formats, ce qui en fait un outil précieux pour les projets de développement.

## Comment faire

Pour travailler avec des fichiers CSV en Kotlin, nous allons utiliser la bibliothèque open source kotlin-CSV. Cette bibliothèque offre des fonctionnalités pour lire, écrire et modifier des fichiers CSV de manière simple et efficace.

Tout d'abord, nous devons ajouter la dépendance de kolin-CSV à notre projet dans le fichier build.gradle :

```Kotlin
dependencies {
    implementation 'com.github.doyaaaaaken:kotlin-csv-jvm:0.15.0'
}
```

Ensuite, nous importons la bibliothèque dans notre fichier Kotlin :

```Kotlin
import com.github.doyaaaaaken.kotlincsv.dsl.csvReader
```

Maintenant, voyons comment lire un fichier CSV et afficher son contenu :

```Kotlin
val input = File("example.csv")
// Créez un objet de lecteur CSV en utilisant le fichier d'entrée
val csvReader = csvReader { delimiter = ',' }
// Lisez chaque ligne du fichier et affichez-la en utilisant une boucle for
csvReader.open(input) {
    readAllAsSequence().forEach { row ->
        // Accédez aux valeurs en utilisant les index ou les noms de colonnes
        println("Nom: ${row[0]}, Âge: ${row[1]}, Ville: ${row["ville"]}")
    }
}
```

Voici la sortie que nous obtiendrons en utilisant le contenu du fichier CSV suivant :

```
Nom,Âge,Ville
Marie,25,Montreal
Pierre,30,Paris
Emma,22,Lyon
```

```
Nom: Marie, Âge: 25, Ville: Montreal
Nom: Pierre, Âge: 30, Ville: Paris
Nom: Emma, Âge: 22, Ville: Lyon
```

Maintenant que nous savons comment lire un fichier CSV, voyons comment écrire et modifier des données existantes. Supposons que nous voulons ajouter une nouvelle colonne "Profession" à notre fichier CSV et y enregistrer un nouvel enregistrement. Voici comment nous pouvons le faire :

```Kotlin
val output = File("new_example.csv")
val newRecord = listOf("Sophie", "26", "Toronto", "Infirmière")
// Utilisez le mode d'écriture et ajoutez le nouvel enregistrement à la fin du fichier
// N'oubliez pas d'ajouter un retour à la ligne après chaque enregistrement
csvReader.open(input, mode = CsvMode.Write, append = true) {
   writeRow(newRecord)
}
```

## Deep Dive

En travaillant avec des fichiers CSV, il est important de garder à l'esprit quelques considérations. Tout d'abord, assurez-vous que vos données sont correctement formatées, sinon vous risquez d'avoir des problèmes lors de la lecture et de l'écriture de votre fichier. Ensuite, gardez à l'esprit que le format CSV peut varier selon les applications et les systèmes d'exploitation, donc il est important de tester votre code avec différentes sources et formats de fichier CSV.

See Also : n'oubliez pas de consulter la documentation officielle de kolin-CSV pour en savoir plus sur ses fonctionnalités et ses options de configuration. Vous pouvez également consulter ces autres ressources pour en savoir plus sur la manipulation de fichiers CSV en Kotlin :

- [Kotlin-CSV GitHub Repository](https://github.com/doyaaaaaken/kotlin-csv)
- [How to Read and Write CSV Files in Kotlin](https://www.baeldung.com/kotlin/csv)

Nous espérons que cet article vous a été utile et que vous êtes maintenant prêt à travailler avec des fich