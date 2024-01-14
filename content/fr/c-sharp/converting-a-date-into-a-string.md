---
title:                "C#: Transformer une date en une chaîne de caractères."
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi convertir une date en chaîne de caractères ?

Il existe différentes raisons pour lesquelles vous pourriez avoir besoin de convertir une date en chaîne de caractères dans votre code C#. Par exemple, vous pourriez avoir besoin d'afficher la date dans un format spécifique, de l'enregistrer dans une base de données ou de l'envoyer dans une requête API. La conversion d'une date en chaîne de caractères vous permet de manipuler les informations de date de manière plus flexible et de les adapter à vos besoins spécifiques.

# Comment faire ?

Pour convertir une date en chaîne de caractères en C#, vous pouvez utiliser la méthode `ToString()` de l'objet `DateTime`. Cette méthode accepte un paramètre de format qui vous permet de spécifier comment la date doit être représentée en chaîne de caractères. Par exemple, si vous voulez afficher la date en format jour-mois-année, vous pouvez utiliser le code suivant :

```C#
DateTime date = new DateTime(2021, 10, 20);
string dateAsString = date.ToString("dd-MM-yyyy");
Console.WriteLine(dateAsString);
```

Cela affichera "20-10-2021" dans la console. Vous pouvez également utiliser n'importe quel format de chaîne de date prédéfini dans C# en utilisant le caractère `"d"` suivi du format souhaité. Voici un exemple avec le format complet de date :

```C#
DateTime date = new DateTime(2021, 10, 20);
string dateAsString = date.ToString("d");
Console.WriteLine(dateAsString);
```

Cela affichera "10/20/2021" dans la console, en utilisant le format par défaut pour la culture actuelle.

# Plongée en profondeur

Il est important de noter que la méthode `ToString()` de l'objet `DateTime` est sensible à la culture. Cela signifie que le format de la date sera différent selon la culture de la machine sur laquelle le code s'exécute. Par exemple, si vous utilisez le format "d" sur une machine avec la culture française, vous obtiendrez "20/10/2021", tandis que sur une machine avec la culture américaine, vous obtiendrez "10/20/2021". Il est donc recommandé de spécifier explicitement la culture souhaitée lors de la conversion d'une date en chaîne de caractères.

De plus, vous pouvez également utiliser la méthode `ToString()` pour convertir une date dans n'importe quel autre format de date prédéfini ou même un format personnalisé, en utilisant les caractères spéciaux pour représenter chaque partie de la date, comme "dd" pour le jour, "MM" pour le mois ou "yyyy" pour l'année.

# Voir aussi

- [Documentation officielle de la méthode `ToString()` de l'objet `DateTime`](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.tostring)
- [Différents formats de chaînes de date en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/custom-date-and-time-format-strings)