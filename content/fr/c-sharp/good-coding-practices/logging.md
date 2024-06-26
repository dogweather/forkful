---
date: 2024-01-26 01:00:59.806423-07:00
description: "Comment faire : En C#, vous pouvez utiliser l'espace de noms int\xE9\
  gr\xE9 `System.Diagnostics` ou des biblioth\xE8ques tierces comme NLog ou log4net.\
  \ Voici un\u2026"
lastmod: '2024-03-13T22:44:57.794481-06:00'
model: gpt-4-1106-preview
summary: "En C#, vous pouvez utiliser l'espace de noms int\xE9gr\xE9 `System.Diagnostics`\
  \ ou des biblioth\xE8ques tierces comme NLog ou log4net."
title: Journalisation
weight: 17
---

## Comment faire :
En C#, vous pouvez utiliser l'espace de noms intégré `System.Diagnostics` ou des bibliothèques tierces comme NLog ou log4net. Voici un exemple rapide en utilisant l'interface `ILogger` disponible dans .NET Core :

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("Ceci est un message informatif.");
        logger.LogWarning("Ceci est un message d'avertissement.");
        logger.LogError("Ceci est un message d'erreur.");
    }
}
```

Sortie d'exemple :
```
info: Program[0]
      Ceci est un message informatif.
warn: Program[0]
      Ceci est un message d'avertissement.
fail: Program[0]
      Ceci est un message d'erreur.
```

## Plongée en profondeur
L'histoire de la journalisation dans le développement logiciel est presque aussi ancienne que la programmation elle-même ; elle a évolué de simples instructions d'impression à des systèmes sophistiqués et configurables. À l'origine, la journalisation était réalisée en écrivant dans des fichiers ou la console, mais cela s'est développé pour inclure des structures plus complexes comme les systèmes d'agrégation de logs et les plateformes de traçage distribué (comme la pile ELK ou Jaeger).

Les alternatives à la journalisation intégrée dans .NET comprennent des bibliothèques tierces :
- **NLog** : polyvalent et facile à configurer, avec de nombreuses fonctionnalités pour le routage, le formatage et le filtrage des journaux.
- **log4net** : inspiré par la bibliothèque log4j Java, il est hautement configurable en XML et prend en charge une variété de dépôts de logs.

Quand il s'agit de détails d'implémentation, le choix de votre abstraction de journalisation (comme Microsoft.Extensions.Logging) et le fournisseur de journalisation sous-jacent peuvent affecter de manière significative la performance et la fiabilité de votre application. Il est crucial de configurer les niveaux de journalisation de manière appropriée et de s'assurer que l'écriture des journaux ne devienne pas un goulot d'étranglement.

Aussi, la journalisation structurée - où vous enregistrez non seulement des chaînes mais aussi des paires clé-valeur ou des objets - permet des logs plus précis et exploitables, qui sont plus faciles à interroger et à analyser.

## Voir aussi
- [Documentation de Microsoft.Extensions.Logging](https://docs.microsoft.com/fr-fr/aspnet/core/fundamentals/logging/)
- [Documentation de NLog](https://nlog-project.org/documentation/)
- [Documentation de log4net](https://logging.apache.org/log4net/)
- [Documentation de Serilog](https://serilog.net/) (pour un exemple de journalisation structurée)
