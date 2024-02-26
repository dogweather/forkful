---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:30.644052-07:00
description: "Les fichiers CSV (Comma-Separated Values, ou valeurs s\xE9par\xE9es\
  \ par des virgules) sont un format d'\xE9change de donn\xE9es courant qui repr\xE9\
  sente des donn\xE9es\u2026"
lastmod: '2024-02-25T18:49:54.537213-07:00'
model: gpt-4-0125-preview
summary: "Les fichiers CSV (Comma-Separated Values, ou valeurs s\xE9par\xE9es par\
  \ des virgules) sont un format d'\xE9change de donn\xE9es courant qui repr\xE9sente\
  \ des donn\xE9es\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les fichiers CSV (Comma-Separated Values, ou valeurs séparées par des virgules) sont un format d'échange de données courant qui représente des données tabulaires en texte brut, en utilisant des virgules pour séparer les valeurs individuelles. Les programmeurs travaillent avec des fichiers CSV pour importer, exporter et manipuler des données facilement à travers diverses applications et services, car c'est un format simple, largement pris en charge, compatible avec les applications de feuilles de calcul, les bases de données et les langages de programmation.

## Comment faire :
Travailler avec des fichiers CSV en C# peut être accompli à travers l'espace de noms `System.IO` pour les opérations de base, et pour des manipulations plus complexes ou pour gérer des fichiers plus grands sans problème, on pourrait envisager des bibliothèques tierces comme `CsvHelper`. Ci-dessous sont des exemples de comment lire et écrire des fichiers CSV en utilisant ces deux approches.

### Lire un fichier CSV en utilisant System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"chemin\vers\votre\fichier.csv";
        // Lire toutes les lignes du fichier CSV
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Première Colonne : {rowData[0]}, Deuxième Colonne : {rowData[1]}");
        }
    }
}
```

**Sortie exemple :**
```
Première Colonne : Nom, Deuxième Colonne : Âge
Première Colonne : John Doe, Deuxième Colonne : 30
```

### Écrire dans un fichier CSV en utilisant System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"chemin\vers\votre\sortie.csv";
        var lignes = new List<string>
        {
            "Nom,Âge",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lignes);
        Console.WriteLine("Fichier CSV écrit.");
    }
}
```

**Sortie exemple :**
```
Fichier CSV écrit.
```

### Utiliser CsvHelper pour lire le CSV
Pour utiliser CsvHelper, ajoutez d'abord le paquet `CsvHelper` à votre projet en utilisant le Gestionnaire de Paquets NuGet.

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class ReadCSVWithCsvHelper
{
    static void Main()
    {
        string filePath = @"chemin\vers\votre\fichier.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var enregistrements = csv.GetRecords<dynamic>().ToList();
            foreach (var enregistrement in enregistrements)
            {
                Console.WriteLine($"Première Colonne : {enregistrement.Name}, Deuxième Colonne : {enregistrement.Age}");
            }
        }
    }
}
```

**Sortie exemple :**
```
Première Colonne : John Doe, Deuxième Colonne : 30
Première Colonne : Jane Smith, Deuxième Colonne : 25
```

### Utiliser CsvHelper pour écrire le CSV
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }

    static void Main()
    {
        string filePath = @"chemin\vers\votre\sortie.csv";
        var enregistrements = new List<Person>
        {
            new Person { Name = "John Doe", Age = 30 },
            new Person { Name = "Jane Smith", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(enregistrements);
        }
        
        Console.WriteLine("Fichier CSV écrit avec CsvHelper.");
    }
}
```

**Sortie exemple :**
```
Fichier CSV écrit avec CsvHelper.
```
