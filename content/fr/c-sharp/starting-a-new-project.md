---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Démarrer un nouveau projet est comme créer une toile vierge sur laquelle vous pouvez exprimer vos idées de programmation. Les programmeurs font ceci pour lancer des solutions innovantes, adapter des idées existantes à de nouvelles utilisations, ou simplement pour améliorer leurs compétences.

## Comment faire:

Créer un nouveau projet en C# n'est pas compliqué. Voici quelques étapes simples à suivre.

```C#
using System;
namespace MonProjet
{
   class Programme
   {
      static void Main(string[] args)
      {
         Console.WriteLine("Bonjour le monde!");
      }
   }
}
```
Ceci est un exemple simple d'un nouveau projet C#. Pour exécuter ce code, vous pouvez simplement utiliser la commande `dotnet run` dans votre terminal / console.

## Plongée en profondeur:

Lancer un nouveau projet peut avoir différentes connotations pour différents programmeurs. Historiquement, cela signifie souvent initialiser une nouvelle base de code et créer des bibliothèques, mais dans le contexte des langages modernes comme C#, cela peut signifier simplement créer un nouveau 'namespace'.

Il existe plusieurs manières alternatives de démarrer un projet, par exemple en utilisant des modèles de projet préexistants fournis par l'IDE ou le cadre de travail. Avec l'avènement des frameworks comme .NET Core, vous pouvez même créer des projets à partir du terminal avec la commande `dotnet new`.

Néanmoins, toutes ces approches ont un point commun: elles vous permettent de construire votre propre structure à partir de zéro, de choisir les bibliothèques et les dépendances, et de contrôler tous les aspects de votre environnement de développement.

## Voir aussi:

1. [Documentation officielle Microsoft pour commencer](https://docs.microsoft.com/fr-fr/dotnet/csharp/)
2. [Introduction aux namespaces en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/namespaces/)
3. [Travailler avec des projets et des solutions dans Visual Studio](https://docs.microsoft.com/fr-fr/visualstudio/ide/quickstart-projects-solutions?view=vs-2019)
4. [Créer un nouveau projet avec `dotnet new`](https://docs.microsoft.com/fr-fr/dotnet/core/tools/dotnet-new)