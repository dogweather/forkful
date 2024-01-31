---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire sur l'erreur standard, c'est envoyer des messages d'erreur ou des diagnostics à un flux spécifique, distinct de la sortie standard. Les programmeurs le font pour séparer les erreurs des données de sortie normales, ce qui simplifie le débogage et la gestion des erreurs par les utilisateurs ou d'autres programmes.

## Comment faire :

```C#
using System;

class ErrorLoggingExample 
{
    static void Main() 
    {
        Console.WriteLine("C'est un message standard.");
        Console.Error.WriteLine("Oops, une erreur est survenue!");

        // Utilisez Try-Catch pour capturer les exceptions et les écrire sur stderr.
        try
        {
            // Simulez une opération pouvant échouer...
            throw new Exception("Un problème spécifique est apparu.");
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Erreur : {ex.Message}");
        }
    }
}
```
Sortie :
```
C'est un message standard.
Oops, une erreur est survenue!
Erreur : Un problème spécifique est apparu.
```

## Plongée profonde :

Historiquement, l'erreur standard et la sortie standard étaient des concepts utilisés par Unix comme canaux de communication. En C#, `Console.Error` est une instance de `TextWriter`, et on l'utilise pour écrire des erreurs sans interférer avec la sortie standard `Console.Out`. Dans certains cas, on pourrait préférer des solutions de journalisation plus avancées comme `log4net` ou `NLog`. Leurs avantages incluent la configuration de la sévérité des messages et une facilité de maintenance. Côté implémentation, écrire sur `Console.Error` est aussi simple que sur `Console.Out`, mais il est important de gérer cela correctement, surtout pour les applications qui se composent avec d'autres outils via les pipelines de shell.

## Voir aussi :

- Documentation Microsoft sur `Console.Error`: https://docs.microsoft.com/fr-fr/dotnet/api/system.console.error
- Introduction aux flux standard (en anglais): https://en.wikipedia.org/wiki/Standard_streams
- Guide sur la gestion des erreurs en C# : https://docs.microsoft.com/fr-fr/dotnet/csharp/fundamentals/exceptions/exception-handling
- Pour aller plus loin dans la journalisation : https://nlog-project.org/ et https://logging.apache.org/log4net/
