---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Article: Comparer deux dates en C#

## Quoi & Pourquoi?
Comparer deux dates, c'est déterminer leur ordre chronologique. Les programmeurs le font pour exécuter des opérations basées sur la date comme le tri ou pour vérifier l'échéance.

## Comment faire:
Voici un exemple de la façon de comparer deux dates en C#:

```C#
DateTime date1 = new DateTime(2021, 12, 25);
DateTime date2 = new DateTime(2022, 1, 1);

if (date1 < date2)
{
    Console.WriteLine("date1 est avant date2");
}
else if (date1 > date2)
{
    Console.WriteLine("date1 est après date2");
}
else
{
    Console.WriteLine("date1 est la même que date2");
}
```

Dans cet exemple, l'output sera:

```C#
"date1 est avant date2"
```

## Plongée profonde:
Le support pour la comparaison de dates a été introduit en C# dès sa première version. Vous pouvez également utiliser la méthode `DateTime.Compare` qui renvoie -1, 0, ou 1 basé sur l'ordre des deux dates. Les Détails de mise en œuvre simples sont que les dates sont stockées comme des valeurs numériques qui représentent les ticks (100 nano secondes) depuis une date de base (1er janvier 1), leur comparaison revient à comparer deux nombres.

```C#
int result = DateTime.Compare(date1, date2);
```

## Voir aussi:
- Documentation sur la classe DateTime: https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-6.0
- Méthode DateTime.Compare: https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.compare?view=net-6.0
- Autre discussion sur le sujet: https://stackoverflow.com/questions/2719437/datetime-compare-function-in-c-sharp