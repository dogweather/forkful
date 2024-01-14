---
title:                "C#: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

# Pourquoi travailler avec des fichiers CSV en C# ?

Les fichiers CSV (Comma-Separated Values) sont un format de fichier couramment utilisé pour stocker des données tabulaires. Ils sont très utiles dans le domaine de la programmation car ils peuvent être facilement lus et écrits par de nombreuses langues, y compris C#. Dans cet article, nous allons explorer pourquoi il est important de savoir travailler avec des fichiers CSV en C#.

## Comment le faire en C# ?

Pour lire un fichier CSV en C#, nous pouvons utiliser la classe `StreamReader` pour ouvrir le fichier et la classe `CsvHelper` pour le traiter. Voici un exemple de code qui lit un fichier CSV avec une en-tête et imprime chaque ligne dans la console :

```
using (var reader = new StreamReader("fichier.csv"))
using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
{
    csv.Read();
    csv.ReadHeader();
    while (csv.Read())
    {
        var colonne1 = csv.GetField("colonne1");
        var colonne2 = csv.GetField("colonne2");
        Console.WriteLine($"{colonne1}, {colonne2}");
    }
}
```

Ensuite, si nous voulons écrire dans un fichier CSV en utilisant C#, nous pouvons utiliser la classe `StreamWriter` et la méthode `WriteField` de la classe `CsvWriter` pour remplir chaque cellule du fichier. Voici un exemple de code qui écrit dans un fichier CSV avec une en-tête et des données :

```
using (var writer = new StreamWriter("fichier.csv"))
using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
{
    csv.WriteHeader<Data>();

    var data = new Data
    {
        Colonne1 = "valeur1",
        Colonne2 = "valeur2",
    };

    csv.NextRecord();
    csv.WriteRecord(data);
}
```

La sortie de ces exemples de code serait la suivante :

```
colonne1, colonne2
valeur1, valeur2
```

## Une plongée plus profonde dans les fichiers CSV

Outre la lecture et l'écriture de fichiers CSV, il existe de nombreuses autres fonctionnalités importantes lors de la manipulation de ces fichiers en C#. Par exemple, la classe `CsvHelper` offre des méthodes pour créer des enregistrements personnalisés, définir des délimiteurs de ligne personnalisés et gérer les valeurs nulles.

De plus, il est également possible de travailler avec des fichiers CSV contenant des données multilingues en utilisant la classe `CsvConfiguration` pour définir des règles spécifiques à chaque langue.

Enfin, il est important de noter qu'il existe de nombreuses bibliothèques tierces pour travailler avec des fichiers CSV en C#, offrant des fonctionnalités avancées telles que la validation de données, l'analyse statistique et la gestion des erreurs.

## Voir aussi

- [Microsoft Docs sur les fichiers CSV en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/file-system/how-to-read-and-write-to-a-newly-created-data-file)
- [Utilisation de CsvHelper pour lire et écrire des fichiers CSV en C#](https://joshclose.github.io/CsvHelper/)
- [Lecteur et écrivain de fichiers CSV en C# avec une bibliothèque tiers](https://www.codeproject.com/Articles/9258/A-Fast-CSV-Reader)