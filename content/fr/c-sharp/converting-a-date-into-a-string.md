---
title:    "C#: Transformer une date en chaîne de caractères"
keywords: ["C#"]
---

{{< edit_this_page >}}

### Pourquoi

Il peut sembler simple de convertir une date en une chaîne de caractères en C#, mais parfois, c'est une tâche nécessaire pour répondre aux besoins de votre application. Que vous souhaitiez afficher la date dans un format spécifique ou la stocker dans une base de données, la conversion d'une date en chaîne de caractères est essentielle pour manipuler des données temporelles dans votre code.

### Comment faire

Nous allons utiliser la méthode `ToString()` pour convertir une date en une chaîne de caractères. Voici un exemple de code qui montre comment convertir la date actuelle en une chaîne de caractères au format "mm/dd/yyyy":

```C#
DateTime dateCourante = DateTime.Now;
string dateEnString = dateCourante.ToString("MM/dd/yyyy");
Console.WriteLine(dateEnString);
```

Cela va produire la sortie suivante:

```
07/12/2021
```

Vous pouvez également utiliser des "placeholders" dans le format pour inclure des éléments tels que l'heure ou le jour de la semaine. Par exemple, voici comment convertir la date actuelle en une chaîne de caractères avec l'heure au format "dd/mm/yyyy HH:mm":

```C#
DateTime dateCourante = DateTime.Now;
string dateEnString = dateCourante.ToString("dd/MM/yyyy HH:mm");
Console.WriteLine(dateEnString);
```

Cela produira la sortie suivante:

```
12/07/2021 09:34
```

### Plongée en profondeur

Lorsque vous utilisez la méthode `ToString()` pour convertir une date en une chaîne de caractères, il est important de comprendre les différentes options de formatage. Vous pouvez utiliser des symboles spéciaux dans le format pour représenter différentes parties de la date, telles que le mois ou le jour de la semaine. Vous pouvez également utiliser des symboles de ponctuation pour séparer les différentes parties de la date.

Vous pouvez trouver une liste complète des symboles de formatage de date dans la documentation officielle de Microsoft.NET. En comprenant ces symboles, vous pouvez contrôler précisément comment votre date sera convertie en chaîne de caractères.

### Voir aussi

- [Guide de formatage de date en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Méthode ToString() en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.tostring?view=net-5.0)