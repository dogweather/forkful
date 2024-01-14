---
title:    "C#: Comparaison de deux dates"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est fréquent en programmation de devoir comparer deux dates. Cela peut servir dans de nombreuses situations, par exemple pour vérifier si une date est antérieure ou ultérieure à une autre, pour trier une liste de dates ou pour filtrer des données par rapport à une date donnée. Connaitre les différentes manières de comparer deux dates en langage C# peut donc s'avérer très utile.

## Comment faire

Il existe plusieurs façons de comparer deux dates en C#. La plus simple consiste à utiliser la méthode `Compare` de la structure `DateTime`. Cette méthode prend deux paramètres de type `DateTime` et renvoie un entier, qui sera négatif si la première date est antérieure à la seconde, positif si elle est ultérieure et 0 si les deux dates sont identiques.

```C#
// Comparaison de deux dates
DateTime date1 = new DateTime(2020, 10, 1);
DateTime date2 = new DateTime(2020, 10, 2);
int resultat = date1.CompareTo(date2);

Console.WriteLine(resultat); // Sortie : -1
```

Une autre méthode couramment utilisée pour comparer deux dates est `Equals`, également présente dans la structure `DateTime`. Cette méthode prend également un paramètre de type `DateTime` et renvoie un booléen, qui sera `true` si les deux dates sont identiques et `false` sinon.

```C#
// Comparaison de deux dates
DateTime date1 = new DateTime(2020, 10, 1);
DateTime date2 = new DateTime(2020, 10, 2);
bool resultat = date1.Equals(date2);

Console.WriteLine(resultat); // Sortie : false
```

Il est également possible d'utiliser les opérateurs de comparaison (`==`, `!=`, `<`, `>`, `<=`, `>=`) avec des dates en C#, mais attention à prendre en compte l'heure, les minutes, les secondes et les millisecondes si celles-ci sont importantes dans votre comparaison.

```C#
// Comparaison de deux dates avec des opérateurs
DateTime date1 = new DateTime(2020, 10, 1, 12, 30, 0);
DateTime date2 = new DateTime(2020, 10, 1, 12, 30, 30);
bool resultat = date1 <= date2;

Console.WriteLine(resultat); // Sortie : true
```

## Plongée en profondeur

En C#, les dates sont des valeurs structurées et non pas des objets, ce qui signifie qu'elles sont passées par valeur et qu'une nouvelle copie est créée à chaque fois qu'on les assigne à une autre variable. Cela peut entrainer des problèmes de comparaison si l'heure, les minutes, les secondes ou les millisecondes doivent être prises en compte. Pour éviter cela, il est recommandé d'utiliser la méthode `DateTime.Date` qui permet de n'avoir que la date sans l'heure.

```C#
// Comparaison de deux dates en utilisant Date
DateTime date1 = new DateTime(2020, 10, 1, 12, 30, 0);
DateTime date2 = new DateTime(2020, 10, 1, 15, 45, 0);
int resultat = date1.Date.CompareTo(date2.Date);

Console.WriteLine(resultat); // Sortie : 0
```

Il est également important de mentionner que les dates en C# peuvent être affectées par le fuseau horaire du système. Il est donc recommandé d'utiliser la structre `DateTimeOffset` pour gérer les dates et les heures en fonction du fuseau horaire.

## Voir aussi

- [Documentation officielle de la structure DateTime en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=netcore-3.1)
- [Tutoriel pour gérer les dates et heures en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/datetime/)