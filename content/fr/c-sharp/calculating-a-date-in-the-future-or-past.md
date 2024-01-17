---
title:                "Calculer une date dans le futur ou le passé."
html_title:           "C#: Calculer une date dans le futur ou le passé."
simple_title:         "Calculer une date dans le futur ou le passé."
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ? 

Calculer une date dans le futur ou le passé est une tâche courante pour les programmeurs. Cela consiste à manipuler et à traiter des données de date et d'heure afin de déterminer une date spécifique à partir d'une date de référence. Cette pratique est particulièrement utile pour automatiser des tâches telles que la planification et la programmation de rappels ou de notifications.

## Comment faire :

Pour calculer une date dans le futur ou le passé en utilisant le langage C#, il existe plusieurs possibilités. Par exemple, vous pouvez utiliser les fonctions natives de C# telles que `DateTime.Add()` pour ajouter une certaine quantité de temps à une date spécifiée. Voici un exemple de code :

```C#
DateTime date = new DateTime(2021, 1, 1);
 
DateTime futureDate = date.AddYears(5);
 
Console.WriteLine(futureDate); // Output: 01/01/2026
```

Vous pouvez également utiliser la classe `TimeSpan` pour spécifier une période de temps et l'ajouter à une date donnée :

```C#
DateTime date = new DateTime(2021, 1, 1);
 
TimeSpan interval = new TimeSpan(5, 0, 0, 0); // 5 jours
 
DateTime futureDate = date + interval;
 
Console.WriteLine(futureDate); // Output: 01/06/2021
```

De plus, C# offre la possibilité d'utiliser le package `System.Time`, qui fournit des méthodes et des types pour manipuler et effectuer des calculs sur les dates et les heures.

## Approfondissement :

Calculer une date dans le futur ou le passé était une tâche plus complexe avant l'introduction des langages de programmation modernes. Les programmeurs devaient souvent créer leurs propres fonctions et algorithmes pour effectuer cette tâche. De nos jours, il existe de nombreuses bibliothèques et packages qui peuvent simplifier cette tâche, tels que le package `System.Time`, qui offre des fonctions de manipulation de dates et d'heures plus avancées.

Il existe également d'autres alternatives pour calculer une date dans le futur ou le passé, telles que l'utilisation de bibliothèques de calendrier ou de services en ligne.

## Voir aussi :

- [Documentation officielle de Microsoft sur le package System.Time](https://docs.microsoft.com/en-us/dotnet/api/system.time?view=net-5.0)
- [Exemple de manipulation de dates en C#](https://www.codeproject.com/Articles/5786960/How-to-Manipulate-Dates-in-Csharp) 
- [Bibliothèque de calendrier open-source pour .NET](https://github.com/rotorgames/Rotorsoft.Tools.Time)