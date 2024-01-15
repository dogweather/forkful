---
title:                "Transformer une date en chaîne de caractères"
html_title:           "C#: Transformer une date en chaîne de caractères"
simple_title:         "Transformer une date en chaîne de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut arriver que vous ayez besoin de convertir une date en tant que chaîne de caractères dans votre programme en C#. Cela peut être utile pour afficher une date de manière lisible pour l'utilisateur ou pour l'enregistrer dans un fichier de log.

## Comment faire

Pour convertir une date en chaîne de caractères, vous pouvez utiliser la méthode `ToString` de la classe `DateTime`. Cette méthode vous permet de spécifier un format pour votre date en utilisant des codes de format. Voici un exemple de code :

```C#
DateTime date = new DateTime(2021, 10, 15);
string dateString = date.ToString("dd/MM/yyyy");
Console.WriteLine(dateString); // Affiche "15/10/2021"
```

Dans cet exemple, nous avons créé une instance de `DateTime` avec la date du 15 octobre 2021. Nous avons ensuite utilisé la méthode `ToString` pour convertir cette date en une chaîne de caractères au format "jour/mois/année". Vous pouvez utiliser différents codes de format pour obtenir différentes représentations de votre date.

Un autre moyen de convertir une date en chaîne de caractères est d'utiliser la classe `Convert` et sa méthode `ToString`. Voici un exemple :

```C#
DateTime date = new DateTime(2021, 10, 15);
string dateString = Convert.ToString(date);
Console.WriteLine(dateString); // Affiche "15/10/2021 00:00:00"
```

Dans ce cas, la méthode `ToString` de la classe `Convert` utilisera le format par défaut de la machine pour convertir la date en chaîne de caractères.

## Plongée en profondeur

Lorsque vous utilisez la méthode `ToString` pour convertir une date en chaîne de caractères, vous pouvez également spécifier un objet de formatage en utilisant la classe `IFormatProvider`. Cet objet vous permet de spécifier une culture pour la représentation de la date. Cela peut être utile si vous souhaitez afficher la date dans un format différent selon la langue de l'utilisateur de votre programme.

Voici un exemple de code utilisant un `IFormatProvider` :

```C#
DateTime date = new DateTime(2021, 10, 15);
string dateString = date.ToString("D", new CultureInfo("fr-FR"));
Console.WriteLine(dateString); // Affiche "vendredi 15 octobre 2021"
```

Dans cet exemple, nous avons utilisé le code de format "D" qui affiche la date sous forme de jour de la semaine, mois et année. Nous avons également spécifié la culture française pour que la date soit affichée en français.

## Voir aussi

- [Documentation officielle de Microsoft sur la méthode ToString](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.tostring?view=net-5.0)
- [Guide C# sur les formats de date et d'heure](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/custom-date-and-time-format-strings)