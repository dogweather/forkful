---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:39.263891-07:00
description: "Comment faire : En C#, \xE9crire sur l'erreur standard peut \xEAtre\
  \ r\xE9alis\xE9 en utilisant le flux `Console.Error`. Ce flux est utilis\xE9 sp\xE9\
  cifiquement pour les\u2026"
lastmod: '2024-03-13T22:44:57.805615-06:00'
model: gpt-4-0125-preview
summary: "En C#, \xE9crire sur l'erreur standard peut \xEAtre r\xE9alis\xE9 en utilisant\
  \ le flux `Console.Error`."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

## Comment faire :
En C#, écrire sur l'erreur standard peut être réalisé en utilisant le flux `Console.Error`. Ce flux est utilisé spécifiquement pour les messages d'erreur et les diagnostics. Voici un exemple basique :

```csharp
Console.Error.WriteLine("Erreur : Échec du traitement de la demande.");
```

Exemple de sortie (vers stderr) :
```
Erreur : Échec du traitement de la demande.
```

Pour les scénarios où vous pourriez utiliser une bibliothèque tierce offrant des capacités de journalisation avancées, comme `Serilog` ou `NLog`, vous pouvez configurer ces bibliothèques pour écrire les journaux d'erreur vers stderr. Bien que ces exemples se concentrent sur une simple redirection de console, rappelez-vous que dans les applications de production, les cadres de journalisation offrent des options de gestion d'erreur et de sortie beaucoup plus robustes. Voici un exemple simple avec `Serilog` :

D'abord, installez le package Serilog et son évacuation Console :

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

Ensuite, configurez Serilog pour écrire vers stderr :

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("Ceci est un message normal.");
Log.Error("Ceci est un message d'erreur.");
```

Exemple de sortie (vers stderr pour le message d'erreur) :
```
[15:04:20 ERR] Ceci est un message d'erreur.
```

Note : La configuration `standardErrorFromLevel` dans l'évacuation console de Serilog redirige tous les événements de log au niveau spécifié (Erreur, dans ce cas) ou supérieur vers le flux d'erreur standard, tandis que les messages de niveau inférieur comme Information sont écrits vers le flux de sortie standard.
