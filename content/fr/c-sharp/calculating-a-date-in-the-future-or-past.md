---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "C#: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez une application qui nécessite de calculer une date dans le futur ou dans le passé, vous devez savoir comment le faire correctement. Dans cet article, nous allons découvrir comment utiliser C# pour calculer des dates dans le futur ou dans le passé.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant C#, vous avez besoin des fonctions `DateTime.Add()` et `DateTime.Subtract()`. Voici un exemple de code pour calculer une date dans le futur en ajoutant un nombre de jours donné :

```C#
DateTime dateActuelle = DateTime.Now;
int joursFuturs = 10;
DateTime dateFutur = dateActuelle.Add(TimeSpan.FromDays(joursFuturs));

Console.WriteLine($"La date actuelle est : {dateActuelle}");
Console.WriteLine($"Dans {joursFuturs} jours, la date sera : {dateFutur}");
```

Et voici le résultat de ce code :

```
La date actuelle est : 19/06/2021 14:00:00
Dans 10 jours, la date sera : 29/06/2021 14:00:00
```

De même, si vous voulez calculer une date dans le passé, vous pouvez utiliser la fonction `DateTime.Subtract()` en spécifiant un nombre de jours négatif. Voici un exemple :

```C#
DateTime dateActuelle = DateTime.Now;
int joursPasses = -5;
DateTime datePassee = dateActuelle.Subtract(TimeSpan.FromDays(joursPasses));

Console.WriteLine($"La date actuelle est : {dateActuelle}");
Console.WriteLine($"Il y a {joursPasses} jours, la date était : {datePassee}");
```

Et voici le résultat :

```
La date actuelle est : 19/06/2021 14:00:00
Il y a -5 jours, la date était : 14/06/2021 14:00:00
```

Vous pouvez également utiliser les autres unités de temps comme `TimeSpan.FromHours()` pour indiquer un nombre d'heures à ajouter ou à soustraire.

## Plongée en profondeur

Il est important de noter que la fonction `DateTime.Add()` et `DateTime.Subtract()` ne modifient pas l'objet `DateTime` d'origine mais renvoient une nouvelle instance modifiée. De plus, il est possible d'ajouter ou de soustraire des valeurs négatives pour inverser le sens du calcul. Par exemple, `DateTime.Add(-10)` calculera une date dans le passé alors que `DateTime.Subtract(-10)` calculera une date dans le futur.

## Voir aussi

- [Documentation sur DateTime.Add()](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.add)
- [Documentation sur DateTime.Subtract()](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.subtract)