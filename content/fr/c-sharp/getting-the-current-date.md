---
title:    "C#: Obtenir la date actuelle"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

Obtenir la date actuelle peut sembler être une tâche simple, mais cela peut être utile dans de nombreux cas lors de la programmation en C#. Par exemple, vous pourriez avoir besoin d'afficher la date actuelle sur votre application ou de manipuler des données en fonction de la date.

## Comment faire

Pour obtenir la date actuelle en C#, vous pouvez utiliser la méthode `DateTime.Now`. Voici un exemple de code qui vous montre comment utiliser cette méthode :

```C#
DateTime dateActuelle = DateTime.Now;
Console.WriteLine(dateActuelle);
```

La sortie de ce code sera quelque chose comme ceci :

```
2/5/2020 4:26:00 PM
```

Vous pouvez également formater la date en utilisant la méthode `ToString()` en lui passant un format spécifique en tant que paramètre. Par exemple, pour afficher la date au format `dd/MM/yyyy`, vous pouvez utiliser :

```C#
DateTime dateActuelle = DateTime.Now;
Console.WriteLine(dateActuelle.ToString("dd/MM/yyyy"));
```

## Plongée en profondeur

En C#, la classe `DateTime` est utilisée pour représenter des dates et des heures. Elle contient diverses propriétés et méthodes utiles pour manipuler et formater les dates.

La méthode `Now` utilisée dans l'exemple ci-dessus renvoie la date et l'heure actuelles du système. Vous pouvez également utiliser les méthodes `Today` et `UtcNow` pour obtenir respectivement la date actuelle sans l'heure et la date et l'heure actuelles en temps universel.

De plus, vous pouvez utiliser la méthode `Parse()` pour convertir une chaîne en une date, et la méthode `ParseExact()` pour convertir une chaîne en une date en utilisant un format spécifique.

## Voir aussi

- [Documentation officielle de Microsoft sur la classe DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=netcore-3.1)
- [Tutoriel sur la manipulation des dates en C#](https://www.c-sharpcorner.com/article/date-manipulation-in-c-sharp/)
- [Vidéo YouTube sur l'utilisation de la classe DateTime en C#](https://www.youtube.com/watch?v=bkSxMjXpCbA)