---
title:    "C#: Calculer une date dans le futur ou le passé"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi 
Il y a de nombreuses raisons pour lesquelles quelqu'un pourrait vouloir calculer une date dans le futur ou dans le passé. Peut-être que vous avez besoin de planifier des projets à une date spécifique ou peut-être que vous voulez simplement savoir quelle sera la date dans un certain nombre de jours. Quelle que soit la raison, il est important de connaître les bases de la programmation pour pouvoir effectuer facilement ces calculs.

## Comment faire 
Pour calculer une date dans le futur ou dans le passé en utilisant le langage de programmation C#, vous aurez besoin de quelques connaissances de base en matière de traitement de dates et d'heures. Tout d'abord, vous devrez importer l'espace de noms System afin d'utiliser la classe DateTime. Ensuite, vous pouvez utiliser différentes méthodes de cette classe pour effectuer des calculs sur les dates.

Par exemple, pour calculer une date dans le futur en ajoutant un nombre de jours donné à la date actuelle, vous pouvez utiliser la méthode AddDays() en spécifiant le nombre de jours à ajouter. Le code suivant illustre cela :

```C#
using System;

DateTime currentDate = DateTime.Now;
DateTime futureDate = currentDate.AddDays(10);
Console.WriteLine("La date dans 10 jours sera : {0:d}", futureDate);
```

Lorsque vous exécutez ce code, la sortie devrait ressembler à ceci :

```
La date dans 10 jours sera : 5/6/2021
```

Vous pouvez également utiliser d'autres méthodes telles que AddWeeks(), AddMonths() ou même AddYears() pour effectuer des calculs sur les dates. De plus, si vous voulez calculer une date dans le passé, vous pouvez utiliser les mêmes méthodes en spécifiant un nombre négatif.

## Plongée en profondeur 
Il est important de noter que ces méthodes de calcul de dates prennent en compte les années bissextiles et les différents nombres de jours dans les mois. En outre, vous pouvez également utiliser la classe Calendar pour prendre en compte les cultures et les calendriers spécifiques.

De plus, la classe DateTime dispose également de méthodes utiles telles que Compare(), Equals() et CompareTo(), qui vous permettent de comparer des dates et de vérifier si elles sont égales ou quelles sont leurs valeurs relatives.

Enfin, il existe d'autres façons de calculer des dates en utilisant des bibliothèques tierces ou même des algorithmes complexes, mais la maîtrise de ces méthodes de base est essentielle dans la programmation C#.

## Voir aussi 
- [Documentation officielle sur la classe DateTime en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-5.0)
- [Guide de formatage de dates et d'heures en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Tutoriel sur le calcul de dates en C#](https://www.codeproject.com/Articles/45018/Calculating-Dates-in-C-NET)