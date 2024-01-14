---
title:                "C#: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous que l'une des tâches les plus courantes en programmation est de récupérer la date actuelle ? Que ce soit pour enregistrer l'heure à laquelle un événement s'est produit, pour afficher la date dans une application ou simplement pour des besoins de débogage, il est essentiel de savoir comment récupérer la date actuelle en C#. Dans cet article, nous allons explorer différentes façons de le faire.

## Comment faire

Il existe plusieurs façons de récupérer la date actuelle en C#. La plus simple consiste à utiliser la méthode `DateTime.Now` qui renvoie un objet `DateTime` représentant la date et l'heure actuelles. Voici un exemple de code :

```C#
DateTime dateActuelle = DateTime.Now;
Console.WriteLine(dateActuelle);
```

Cela affichera la date et l'heure actuelles dans le format par défaut de votre système d'exploitation. Si vous souhaitez afficher la date dans un format spécifique, vous pouvez utiliser la méthode `ToString()` en passant un format en paramètre. Par exemple :

```C#
DateTime dateActuelle = DateTime.Now;
Console.WriteLine(dateActuelle.ToString("dd/MM/yyyy"));
```

Cela affichera la date actuelle au format "jour/mois/année".

Vous pouvez également récupérer des informations spécifiques à partir de l'objet `DateTime`, telles que le jour de la semaine, le mois ou l'année. Par exemple :

```C#
DateTime dateActuelle = DateTime.Now;
Console.WriteLine(dateActuelle.DayOfWeek); // affiche le jour de la semaine (en anglais)
Console.WriteLine(dateActuelle.Month); // affiche le mois actuel (numéro)
Console.WriteLine(dateActuelle.Year); // affiche l'année actuelle (numéro)
```

## Deep Dive

Lorsque vous appelez la méthode `DateTime.Now`, vous récupérez la date et l'heure actuelles en utilisant le fuseau horaire de votre système d'exploitation. Cependant, en C#, vous pouvez également récupérer la date et l'heure dans d'autres fuseaux horaires en utilisant les méthodes `DateTime.UtcNow` (fuseau horaire universel) ou `DateTime.Offset` (fuseau horaire spécifique). Vous pouvez également convertir une date et une heure dans un autre fuseau horaire en utilisant la classe `TimeZoneInfo`.

De plus, en utilisant la classe `DateTimeOffset` au lieu de `DateTime`, vous pouvez stocker la date et l'heure avec des informations de fuseau horaire, ce qui est utile si vous travaillez avec des dates et des heures dans différents fuseaux horaires.

## Voir aussi

- [Documentation Microsoft sur la classe DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime)
- [Tutoriel sur la gestion des dates et heures en C#](https://www.c-sharpcorner.com/uploadfile/john_charles/date-time-operations-in-C-Sharp/)
- [Guide de référence rapide sur les formats de date en C#](https://www.c-sharpcorner.com/article/formatting-types-in-net-console/)