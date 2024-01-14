---
title:    "C#: Calculer une date dans le futur ou le passé"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Pourquoi

La calcul de dates dans le futur ou dans le passé est une tâche courante dans la programmation. En utilisant le langage de programmation C#, vous pouvez facilement faire des calculs précis pour des dates spécifiques.

# Comment faire

Pour calculer une date dans le futur ou dans le passé, vous pouvez utiliser la méthode *Add* de la classe *DateTime*. Cette méthode prend un paramètre de type *TimeSpan* qui détermine la quantité de temps à ajouter ou à soustraire à la date d'origine.

Voici un exemple de code pour calculer une date dans le futur:

```C#
// Date d'origine
DateTime dateOriginale = new DateTime(2021, 1, 1);
// Ajout d'un nombre donné de jours
DateTime dateFutur = dateOriginale.Add(new TimeSpan(30, 0, 0, 0));

Console.WriteLine(dateFutur);
// Résultat: 2021-01-31 (31 janvier 2021)
```

Pour calculer une date dans le passé, il suffit de passer un nombre négatif à la méthode *Add*.

```C#
// Date d'origine
DateTime dateOriginale = new DateTime(2021, 1, 1);
// Soustraction d'un nombre donné de jours
DateTime datePassé = dateOriginale.Add(new TimeSpan(-30, 0, 0, 0));

Console.WriteLine(datePassé);
// Résultat: 2020-12-02 (2 décembre 2020)
```

# Plongée en profondeur

En utilisant la méthode *Add*, vous pouvez également effectuer des calculs avec d'autres unités de temps telles que les heures, les minutes, les secondes, etc. De plus, il est important de comprendre la différence entre un *TimeSpan* positif et négatif lors du calcul d'une date dans le futur ou dans le passé.

# Voir aussi

- [Documentation officielle Microsoft pour la classe DateTime (en français)](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-5.0)
- [Tutoriel C# pour les débutants (en français)](https://docs.microsoft.com/fr-fr/dotnet/csharp/tutorials/intro-to-csharp/)
- [Autre exemple de calcul de dates en C# (en français)](https://www.journaldunet.fr/web-tech/internet/1203833-messa-de-najai-ezreal-dans-lol-le-guide/)