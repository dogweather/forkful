---
title:                "Travailler avec les fichiers csv"
html_title:           "C#: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez en tant que développeur ou que vous étudiez la programmation, vous avez probablement déjà entendu parler du format CSV. Les fichiers CSV (Comma-Separated Values) sont un moyen simple et efficace de stocker et d'échanger des données tabulaires. Dans cet article, nous allons explorer comment travailler avec des fichiers CSV en utilisant le langage de programmation C#.

## Comment faire

Pour travailler avec des fichiers CSV en C#, nous allons utiliser deux classes principales : la classe `StreamReader` pour lire le fichier CSV et la classe `StreamWriter` pour écrire des données dans un fichier CSV. Voici un exemple de code qui lit un fichier CSV et affiche son contenu dans la console :

```C#
using System;
using System.IO; 

class Program
{
    static void Main(string[] args)
    {
        // Ouvrir le fichier CSV en utilisant StreamReader
        using (var sr = new StreamReader("mon_fichier.csv"))
        {
            // Lire la première ligne qui contient les en-têtes de colonnes
            string[] headers = sr.ReadLine().Split(',');

            // Tant que nous ne sommes pas à la fin du fichier
            while (!sr.EndOfStream)
            {
                // Lire chaque ligne et la stocker dans un tableau
                string[] rows = sr.ReadLine().Split(',');

                // Afficher les données dans la console
                for (int i = 0; i < headers.Length; i++)
                {
                    Console.Write($"{headers[i]}: {rows[i]} | ");
                }
                Console.WriteLine();
            }
        }
    }
}
```

La sortie de ce code devrait ressembler à ceci :

```
Nom: Jean | Âge: 25 | Sexe: Masculin |
Nom: Marie | Âge: 30 | Sexe: Féminin |
```

Vous pouvez également écrire des données dans un fichier CSV en utilisant la classe `StreamWriter`. Voici un exemple de code qui crée un fichier CSV à partir d'un tableau de données :

```C#
using System;
using System.IO;

class Program
{
    static void Main(string[] args)
    {
        // Les données à écrire dans le fichier CSV
        string[][] data = new string[][]
        {
            new string[] { "Janvier", "Février", "Mars" },
            new string[] { "1000", "2000", "3000" }
        };

        // Ouvrir un fichier CSV en utilisant StreamWriter
        using (var sw = new StreamWriter("nouveau_fichier.csv"))
        {
            // Écrire les en-têtes de colonnes
            foreach (string column in data[0])
            {
                sw.Write($"{column},");
            }
            sw.WriteLine();

            // Écrire chaque ligne de données
            for (int i = 1; i < data.Length; i++)
            {
                foreach (string value in data[i])
                {
                    sw.Write($"{value},");
                }
                sw.WriteLine();
            }
        }
    }
}
```

Cela devrait créer un nouveau fichier CSV contenant les données suivantes :

```
Janvier,Février,Mars,
1000,2000,3000,
```

## Plongée en profondeur

Travailler avec des fichiers CSV peut parfois être un peu délicat car il est important de gérer correctement les caractères spéciaux et les différentes conventions de format. Heureusement, il existe des bibliothèques en C# telles que CsvHelper ou FileHelpers qui facilitent la manipulation de fichiers CSV en fournissant des fonctions pour gérer ces problèmes. N'hésitez pas à les explorer si vous avez besoin d'une solution plus complète et robuste.

## Voir aussi

- [Documentation officielle de Microsoft sur la classe `StreamReader`](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Documentation officielle de Microsoft sur la classe `StreamWriter`](https://docs.microsoft.com/fr-fr/dotnet/api/system.io.streamwriter?view=netcore-3.1)
- [CsvHelper](https://joshclose.github.io/CsvHelper/)
- [FileHelpers](https://www.filehelpers.net/)