---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:20.962435-07:00
description: "Comment faire : Pour v\xE9rifier si une cha\xEEne contient un motif\
  \ sp\xE9cifique, vous pouvez utiliser la m\xE9thode `Regex.IsMatch` de l'espace\
  \ de noms\u2026"
lastmod: '2024-03-13T22:44:57.775943-06:00'
model: gpt-4-0125-preview
summary: "Pour v\xE9rifier si une cha\xEEne contient un motif sp\xE9cifique, vous\
  \ pouvez utiliser la m\xE9thode `Regex.IsMatch` de l'espace de noms `System.Text.RegularExpressions`."
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

## Comment faire :


### Correspondance de motifs simple
Pour vérifier si une chaîne contient un motif spécifique, vous pouvez utiliser la méthode `Regex.IsMatch` de l'espace de noms `System.Text.RegularExpressions`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Bonjour, le monde !";
        string pattern = "monde";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Sortie : True
    }
}
```

### Extraction de données
Extraire des données d'une chaîne à l'aide de groupes dans une regex peut se faire avec la méthode `Regex.Match`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date : 2023-04-12";
        string pattern = @"Date : (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Année : {match.Groups[1].Value}");  // Sortie : Année : 2023
            Console.WriteLine($"Mois : {match.Groups[2].Value}");  // Sortie : Mois : 04
            Console.WriteLine($"Jour : {match.Groups[3].Value}");  // Sortie : Jour : 12
        }
    }
}
```

### Remplacement de texte
La méthode `Regex.Replace` vous permet de remplacer du texte dans une chaîne qui correspond à un motif spécifié.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visitez Microsoft !";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Sortie : Visitez Google !
    }
}
```

### Diviser des chaînes
Vous pouvez diviser une chaîne en un tableau basé sur un motif regex à l'aide de la méthode `Regex.Split`.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "un,deux,trois,quatre,cinq";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Sortie : 
        // un
        // deux
        // trois
        // quatre
        // cinq
    }
}
```

### Utilisation de bibliothèques tierces
Bien que le .NET Framework offre un support étendu pour les expressions régulières, il existe également des bibliothèques tierces telles que `PCRE.NET` qui proposent des expressions régulières compatibles avec Perl (PCRE) en C#. Cela peut être utile si vous avez besoin de fonctionnalités ou d'une syntaxe de l'engine regex de Perl qui ne sont pas disponibles dans l'implémentation de .NET.

Pour utiliser `PCRE.NET`, vous devriez d'abord installer son package NuGet, puis vous pouvez l'utiliser de manière similaire à la manière dont vous utilisez les classes regex natives de .NET.

```csharp
// Exemple utilisant PCRE.NET ici
// Note : Imaginez un exemple similaire à ceux ci-dessus, adapté pour mettre en avant une fonctionnalité unique à PCRE.NET.
```

Lors de l'intégration de bibliothèques tierces pour les expressions régulières, consultez toujours leur documentation pour des informations détaillées sur l'utilisation et la compatibilité.
