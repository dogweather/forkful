---
title:                "Comparer deux dates"
html_title:           "C#: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qu'est-ce que la comparaison de deux dates et pourquoi les programmeurs le font-ils?
La comparaison de deux dates est une méthode utilisée par les programmeurs pour déterminer si une date est antérieure, égale ou postérieure à une autre. Cela peut être utile dans de nombreuses applications, telles que la gestion des tâches, les réservations en ligne, ou simplement pour vérifier la validité d'une entrée utilisateur.

## Comment faire:
Pour comparer deux dates en C#, vous pouvez utiliser la méthode `DateTime.CompareTo()`. Elle renvoie un entier négatif si la première date est antérieure à la seconde, 0 si les deux dates sont égales, ou un entier positif si la première date est postérieure à la seconde. Voici un exemple de code avec un résultat de sortie correspondant:

```C#
DateTime date1 = new DateTime(2021, 01, 01);
DateTime date2 = new DateTime(2021, 02, 01);

int result = date1.CompareTo(date2);
Console.WriteLine(result); // Output: -1
```

## Plongée en profondeur:
La comparaison de dates est importante car elle permet de comparer des événements dans le temps et de prendre des décisions en fonction de ces comparaisons. Il est également important de noter que la méthode `CompareTo()` utilise une comparaison basée sur les ticks, qui mesurent le nombre de 100 nanosecondes écoulées depuis le 1er janvier 0001.

Il existe également d'autres méthodes pour comparer des dates, telles que `DateTime.Equals()` qui vérifie si les deux dates sont exactement les mêmes, ou `DateTime.Compare()` qui renvoie un résultat différent de `CompareTo()` basé sur plusieurs facteurs. 

## Voir aussi:
Pour en savoir plus sur la gestion des dates en C#, vous pouvez consulter la documentation officielle de Microsoft sur les méthodes liées aux dates (https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-5.0). Vous pouvez également découvrir d'autres méthodes de comparaison de dates en faisant des recherches sur Internet ou en consultant des ressources spécifiques au framework que vous utilisez.