---
date: 2024-01-26 04:12:07.576123-07:00
description: "Un REPL, ou Boucle Lire-\xC9valuer-Afficher, vous permet de taper du\
  \ code C# et de l'ex\xE9cuter interactivement. Les programmeurs l'utilisent pour\
  \ des\u2026"
lastmod: '2024-03-11T00:14:31.738902-06:00'
model: gpt-4-0125-preview
summary: "Un REPL, ou Boucle Lire-\xC9valuer-Afficher, vous permet de taper du code\
  \ C# et de l'ex\xE9cuter interactivement. Les programmeurs l'utilisent pour des\u2026"
title: Utilisation d'une console interactive (REPL)
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Un REPL, ou Boucle Lire-Évaluer-Afficher, vous permet de taper du code C# et de l'exécuter interactivement. Les programmeurs l'utilisent pour des expériences rapides, le débogage ou apprendre C#, sans le fardeau de configurer des projets complets.

## Comment faire :
Démarrez un REPL dans votre environnement C# en utilisant la fenêtre C# Interactive ou exécutez `dotnet-script` dans votre terminal. Voici un avant-goût de son utilisation :

```csharp
> var greeting = "Bonjour, REPL !";
> Console.WriteLine(greeting);
Bonjour, REPL !
>
```

Vous obtenez instantanément un retour. Pas de compilation ni d'étape d'exécution. Juste coder et voir.

## Plongée Profonde
Le REPL a voyagé depuis Lisp vers les langues modernes, prospérant dans des langages dynamiques comme Python. Avec C#, Roslyn a rapproché le REPL des développeurs. `csi` pour Roslyn, et `dotnet-script` pour .NET Core, sont des options solides. Une coupe plus profonde : ils évaluent le code ligne par ligne, pas en entier, un modèle d'exécution différent par rapport aux applications C# typiques. Cela impacte la persistance de l'état à travers les exécutions et la portée des variables.

La fenêtre C# Interactive de Visual Studio est un REPL alimenté par Roslyn. Elle dispose d'Intellisense, de multiples références, et du support du package NuGet. Un grand pas par rapport aux premières expériences en ligne de commande.

Pour les langues alternatives, Python utilise `IDLE`, JavaScript a le REPL de Node.js, et F# est livré avec `F# Interactive`. Chacun favorise des boucles de feedback instantané, inestimables pour tester de petits extraits de code ou comprendre les fonctionnalités de la langue.

## Voir Aussi
- [REPL `dotnet-script` pour .NET Core](https://github.com/filipw/dotnet-script)
