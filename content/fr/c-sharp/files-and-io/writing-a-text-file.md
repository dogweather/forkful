---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:31.915841-07:00
description: "\xC9crire un fichier texte en C# implique de cr\xE9er ou de modifier\
  \ programmiquement des fichiers textes sur le syst\xE8me de fichiers - une t\xE2\
  che fondamentale\u2026"
lastmod: '2024-02-25T18:49:54.532962-07:00'
model: gpt-4-0125-preview
summary: "\xC9crire un fichier texte en C# implique de cr\xE9er ou de modifier programmiquement\
  \ des fichiers textes sur le syst\xE8me de fichiers - une t\xE2che fondamentale\u2026"
title: "R\xE9diger un fichier texte"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Écrire un fichier texte en C# implique de créer ou de modifier programmiquement des fichiers textes sur le système de fichiers - une tâche fondamentale pour de nombreuses applications, telles que la journalisation, l'exportation de données, ou la gestion de configuration. Les programmeurs réalisent cette opération pour persister les données entre les sessions, partager des informations à travers les systèmes, ou stocker des sorties lisibles par l'homme.

## Comment faire :
C# simplifie les opérations sur les fichiers avec son espace de noms `System.IO`, fournissant des méthodes directes pour écrire des fichiers textes. Voici comment écrire un fichier texte de base et ajouter du texte à un fichier existant.

### Écrire dans un fichier texte à partir de zéro
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\exemple\ExampleFile.txt";
        string contenu = "Bonjour, monde !";

        // Écrire le contenu dans un nouveau fichier
        File.WriteAllText(filePath, contenu);
        
        Console.WriteLine("Fichier écrit avec succès.");
    }
}
```
**Sortie d'exemple :**
```
Fichier écrit avec succès.
```

### Ajouter du texte à un fichier existant
Si vous souhaitez ajouter du texte à la fin d'un fichier existant, vous pouvez utiliser la méthode `File.AppendAllText`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\exemple\ExampleFile.txt";
        string contenuAdditionnel = "\nAjout de plus de contenu.";

        // Ajouter du contenu au fichier
        File.AppendAllText(filePath, contenuAdditionnel);
        
        Console.WriteLine("Contenu ajouté avec succès.");
    }
}
```
**Sortie d'exemple :**
```
Contenu ajouté avec succès.
```

### Utilisation de bibliothèques tierces : `StreamWriter`
Pour un contrôle plus précis de l'écriture, incluant le vidage automatique et la sélection de l'encodage, utilisez `StreamWriter`.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\exemple\ExampleFile.txt";
        string contenu = "Ceci est un exemple utilisant StreamWriter.";

        // Utiliser StreamWriter pour écrire dans un fichier
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(contenu);
        }
        
        Console.WriteLine("Fichier écrit avec StreamWriter avec succès.");
    }
}
```
**Sortie d'exemple :**
```
Fichier écrit avec StreamWriter avec succès.
```

Chacune de ces approches répond à des besoins différents : les méthodes directes de `File` pour des opérations rapides, et `StreamWriter` pour des scénarios d'écriture plus complexes. Choisissez en fonction de vos besoins spécifiques, en considérant des facteurs tels que la performance et la taille du fichier.
