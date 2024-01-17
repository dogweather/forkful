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

# Qu'est-ce que le CSV et pourquoi les programmeurs l'utilisent-ils?

Le CSV (Comma-Separated Values) est un type de fichier utilisé pour stocker des données sous forme de tableaux. Il est populaire parmi les programmeurs car il est facile à lire et à écrire. Le format CSV est également compatible avec de nombreux langages de programmation, ce qui le rend pratique pour échanger des données entre différentes applications.

# Comment faire:

Voici un exemple de code en C# pour lire un fichier CSV et afficher son contenu:

```
string[] lines = File.ReadAllLines("exemple.csv"); // lire le fichier CSV
foreach (string line in lines) // parcourir les lignes du fichier
{
    string[] values = line.Split(','); // séparer les valeurs par une virgule
    foreach (string value in values) // afficher chaque valeur
    {
        Console.Write(value + " ");
    }
    Console.WriteLine();
}
```

Exemple de contenu du fichier CSV:

```
nom, prénom, âge
Dupont, Jean, 25
Martin, Marie, 30
```

Résultat de l'exécution du code:

```
nom prénom âge
Dupont Jean 25
Martin Marie 30
```

# Plongée en profondeur:

Le CSV a été introduit dans les années 70 comme un moyen de stocker des données structurées dans des fichiers texte simples. À l'époque, les mémoires informatiques étaient coûteuses et le CSV était une alternative économique pour stocker et échanger des données.

Il existe également d'autres formats de fichiers pour stocker des données tabulaires, tels que le JSON et le XML. Cependant, le CSV reste populaire en raison de sa simplicité et de sa compatibilité avec de nombreux langages de programmation.

Pour lire et écrire des fichiers CSV en C#, il existe des bibliothèques telles que CsvHelper et FileHelpers qui facilitent le processus. Les développeurs peuvent également créer leurs propres méthodes pour manipuler les données CSV en utilisant les fonctions de base de manipulation de fichiers en C#.

# Voir aussi:

- [CsvHelper Documentation](https://joshclose.github.io/CsvHelper/)
- [FileHelpers Documentation](https://www.filehelpers.net/documentation/)
- [Working with CSV Files in C#](https://www.c-sharpcorner.com/article/working-with-csv-files-in-C-Sharp/)