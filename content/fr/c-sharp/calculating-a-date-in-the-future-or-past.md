---
title:                "C#: Calculer une date dans le futur ou le passé"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

L'une des tâches les plus courantes en programmation consiste à calculer une date dans le futur ou dans le passé. Cela peut servir dans de nombreux cas, par exemple pour planifier une action, effectuer des opérations financières ou tout simplement pour afficher des informations dynamiques à l'utilisateur.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en C#, nous pouvons utiliser la classe `DateTime` et ses différentes méthodes et propriétés. Voici un exemple de code qui calcule la date dans trois jours à partir de la date actuelle :

```C#
DateTime aujourdhui = DateTime.Now;
DateTime demain = aujourdhui.AddDays(3);
Console.WriteLine("Dans trois jours, nous serons le {0:d}", demain);
```
Lorsque nous exécutons ce code, nous obtenons le résultat suivant :
```
Dans trois jours, nous serons le 03/09/2020
```
Nous pouvons également calculer une date dans le passé en utilisant la méthode `AddDays()` avec un nombre négatif. Voici un exemple qui calcule la date il y a 5 jours à partir de la date actuelle :

```C#
DateTime aujourdhui = DateTime.Now;
DateTime ilYa5Jours = aujourdhui.AddDays(-5);
Console.WriteLine("Il y a cinq jours, c'était le {0:d}", ilYa5Jours);
```
Lorsque nous exécutons ce code, nous obtenons le résultat suivant :
```
Il y a cinq jours, c'était le 29/08/2020
```

## Plongée en profondeur

Il existe de nombreuses autres méthodes et propriétés de la classe `DateTime` qui peuvent nous aider à calculer des dates dans le futur ou dans le passé. Par exemple, nous pouvons utiliser la méthode `AddMonths()` pour ajouter ou soustraire un certain nombre de mois à une date, ou encore la méthode `AddYears()` pour ajouter ou soustraire un certain nombre d'années.

De plus, la classe `DateTime` dispose d'une propriété `Now` qui nous permet d'obtenir la date et l'heure actuelles, et d'une propriété `Today` qui nous renvoie la date actuelle sans l'heure. Nous pouvons également utiliser la méthode `Parse()` pour convertir une chaîne de caractères en objet `DateTime`.

Avec ces outils, nous pouvons facilement calculer toutes sortes de dates dans le futur ou dans le passé en fonction de nos besoins.

## Voir aussi

- [Cours de C# sur OpenClassrooms](https://openclassrooms.com/fr/courses/4875796-programmez-en-oriente-objet-avec-c)
- [Documentation Microsoft sur la classe DateTime en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=netcore-3.1)