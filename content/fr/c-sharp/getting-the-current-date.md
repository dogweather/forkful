---
title:                "Obtenir la date actuelle"
html_title:           "C#: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Bien que cela puisse sembler banal, avoir la date actuelle est une fonctionnalité cruciale dans les applications informatiques. Elle permet de suivre le temps écoulé depuis une certaine date, d'afficher la date d'une action ou simplement d'ajouter une touche de personnalisation à votre application.

## Comment faire

Via la classe `DateTime`, la méthode `Today` retourne la date actuelle en tant qu'objet `DateTime`. Il suffit donc de l'appeler dans notre code :

```C#
DateTime dateActuelle = DateTime.Today;
```

La date actuelle peut également être formatée avec la méthode `ToString()`, en utilisant une chaîne de format spécifique. Par exemple, pour afficher la date au format jour-mois-année :

```C#
string formatDate = "dd-MM-yyyy";
string dateFormatee = dateActuelle.ToString(formatDate);
```

Lors de l'exécution de ces lignes de code, la variable `dateFormatee` contiendra la date actuelle au format précisé.

## Analyse en profondeur

Il existe plusieurs méthodes pour obtenir la date et l'heure actuelles en C#. En plus de `DateTime.Today`, nous pouvons également utiliser `DateTime.Now` pour obtenir l'heure en plus de la date, ou `DateTime.UtcNow` pour obtenir l'heure UTC.

De plus, la classe `DateTime` contient de nombreuses autres méthodes utiles pour formater et manipuler les dates et heures, telles que `AddDays()`, `AddMonths()` et `AddYears()` pour ajouter des jours, mois et années à une date donnée.

Enfin, il est important de noter que la valeur de la date et de l'heure actuelles dépendent de l'horloge système de l'appareil. Il est donc important de s'assurer que l'horloge est correctement réglée pour obtenir des résultats précis.

## Voir aussi

- La documentation officielle de Microsoft sur la classe `DateTime` : https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime
- Un tutoriel complet sur la manipulation des dates et heures en C# : https://www.tutorialspoint.com/csharp/csharp_date_time.htm