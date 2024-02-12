---
title:                "Lancement d'un nouveau projet"
aliases:
- /fr/c-sharp/starting-a-new-project.md
date:                  2024-01-20T18:02:55.198465-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lancement d'un nouveau projet"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? / Quoi et Pourquoi ?
Démarrer un nouveau projet C#, c’est créer une base pour transformer vos idées en code opérationnel. Les programmeurs se lancent dans de nouveaux projets pour résoudre des problèmes, exploiter des nouvelles idées ou apprendre de nouvelles techniques.

## How to / Comment faire :
Créons un projet console simple. Vous avez besoin de .NET SDK installé.

```C#
// Ouvrez le terminal et exécutez :
dotnet new console -o MonProjetConsole
cd MonProjetConsole
dotnet run

// Vous devriez voir :
Hello, World!
```

## Deep Dive / Plongée en Profondeur
Historiquement, les projets C# étaient souvent créés avec Visual Studio, mais .NET Core et maintenant .NET 5/6/7 ont introduit une manière plus légère et cross-platform avec `dotnet` CLI. Alternativement, vous pouvez utiliser des IDEs comme Visual Studio, Visual Studio Code avec l'extension C# ou JetBrains Rider. Pour la structure, un projet console est plus simple, mais pour des applications web, des APIs, ou des apps mobiles, envisagez `dotnet new web`, `dotnet new webapi`, ou `dotnet new maui`.

## See Also / Voir Aussi
- Documentation .NET: https://docs.microsoft.com/fr-fr/dotnet/core/tools/dotnet-new
- Introduction à C#: https://docs.microsoft.com/fr-fr/dotnet/csharp/
- Tutoriels Visual Studio: https://docs.microsoft.com/fr-fr/visualstudio/get-started/csharp/tutorial-console
