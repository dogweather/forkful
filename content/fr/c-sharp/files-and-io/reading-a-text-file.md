---
date: 2024-01-20 17:54:06.503662-07:00
description: "Lire un fichier texte, c'est parcourir son contenu avec votre programme.\
  \ On le fait pour r\xE9cup\xE9rer des donn\xE9es, les analyser ou les transformer."
lastmod: '2024-03-13T22:44:57.806717-06:00'
model: gpt-4-1106-preview
summary: Lire un fichier texte, c'est parcourir son contenu avec votre programme.
title: Lecture d'un fichier texte
weight: 22
---

## What & Why? - Quoi et Pourquoi ?
Lire un fichier texte, c'est parcourir son contenu avec votre programme. On le fait pour récupérer des données, les analyser ou les transformer.

## How to: - Comment faire :
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\exemple.txt";

        // Utiliser 'File.ReadAllText' pour lire tout le fichier
        string content = File.ReadAllText(filePath);
        Console.WriteLine(content);

        // Ou 'File.ReadAllLines' si vous voulez un tableau de lignes
        string[] lines = File.ReadAllLines(filePath);
        foreach(var line in lines)
        {
            Console.WriteLine(line);
        }

        // Ou 'StreamReader' pour plus de contrôle
        using(StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}

```
Sortie d’exemple (dépend du contenu du fichier) :
```
Bonjour, voici un exemple de fichier texte.
Il peut comprendre plusieurs lignes.
Et chaque ligne sera lue individuellement.
```

## Deep Dive - Plongée en profondeur
La lecture de fichiers textes n'est pas une nouveauté. Elle remonte aux premiers jours de la programmation. Avec C#, on a simplifié le processus au fil des ans. 

On peut utiliser `File.ReadAllText` quand on a un fichier court et qu’on veut tout lire d’un seul coup. Pour les fichiers plus longs, `File.ReadAllLines` est sympa, car ça nous donne chaque ligne directement dans un tableau.

`StreamReader`, par contre, est plus flexible. Il gère mieux la mémoire et est plus adapté aux fichiers énormes, car il ne charge pas tout en mémoire à la fois.

Il y a d'autres méthodes, comme les `FileStream` ou les librairies externes pour les besoins spécifiques.

## See Also - Voir Aussi
- Documentation Microsoft sur la lecture des fichiers : [https://docs.microsoft.com/fr-fr/dotnet/standard/io/](https://docs.microsoft.com/fr-fr/dotnet/standard/io/)
