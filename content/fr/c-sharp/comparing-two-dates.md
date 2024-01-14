---
title:                "C#: Comparer deux dates"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Comparer deux dates est une tâche courante en programmation qui peut sembler simple mais qui peut également poser quelques défis. Cet article va vous montrer comment comparer deux dates en utilisant C# et vous donner une compréhension plus approfondie de ce processus.

## Comment faire

Tout d'abord, il est important de comprendre que les dates sont stockées sous forme de valeurs numériques en C#, ce qui signifie qu'elles peuvent facilement être comparées en utilisant des opérateurs de comparaison tels que ">", "<", ">=", "<=".

Voici un exemple de code qui compare deux dates et affiche un message personnalisé en fonction du résultat :

```C#
// Définir deux dates - la date actuelle et le 1er janvier 2021
DateTime currentDate = DateTime.Now;
DateTime newYearDate = new DateTime(2021, 1, 1);

// Comparer les deux dates en utilisant l'opérateur ">"
if (currentDate > newYearDate)
{
    Console.WriteLine("Nous sommes après le 1er janvier 2021 !");
} 
// Afficher un message différent si les dates sont égales
else if (currentDate == newYearDate)
{
    Console.WriteLine("C'est le 1er janvier 2021 !");
}
// Si aucune des conditions ci-dessus n'est remplie, cela signifie que la date actuelle est avant le 1er janvier 2021
else
{
    Console.WriteLine("Nous sommes avant le 1er janvier 2021.");
}
```

Lorsque vous exécutez ce code, vous verrez le message correspondant à la date actuelle affichée dans la console.

Il est également important de noter que les dates peuvent être comparées en utilisant l'objet DateTime lui-même, car il possède une méthode "CompareTo" qui renvoie un entier qui indique si la date est avant, après ou égale à une autre date.

## Plongée en profondeur

Maintenant que vous savez comment comparer deux dates en utilisant des opérateurs ou la méthode "CompareTo", il est utile de comprendre certains des cas spéciaux qui peuvent survenir lors de la comparaison de dates.

Premièrement, les dates peuvent être différentes en termes de fuseau horaire, ce qui peut entraîner des résultats inattendus lors de la comparaison. Dans ces cas, il est recommandé d'effectuer une conversion de fuseau horaire avant de comparer les dates.

Deuxièmement, il y a des cas où des dates peuvent sembler égales en termes de jours, mois et années, mais peuvent être différentes en termes de temps. Dans ces cas, il est important de comparer les dates avec précision, en vérifiant également l'heure et les minutes si nécessaire.

## Voir aussi

- [Documentation Microsoft sur la comparaison de dates en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/types/how-to-compare-dates)
- [Guide de référence sur les opérateurs de comparaison en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/language-reference/operators/comparison-operators)