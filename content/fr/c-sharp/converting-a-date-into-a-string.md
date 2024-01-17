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

Qu'est-ce que la conversion de date en chaîne de caractères en C# et pourquoi les programmeurs le font-ils?

La conversion de date en chaîne de caractères en C# consiste à convertir une date en un format de caractères spécifique, tel que "12/31/2021". Les programmeurs le font pour afficher des dates de manière lisible pour les utilisateurs ou pour stocker des dates sous forme de texte dans une base de données.

Comment faire:

```C#
// Exemple 1 - Conversion de date en chaîne de caractères au format court
DateTime date = new DateTime(2021, 12, 31);
string dateStr = date.ToShortDateString();

Console.WriteLine(dateStr); // Sortie: 12/31/2021

// Exemple 2 - Conversion de date en chaîne de caractères avec un format personnalisé
DateTime date = new DateTime(2021, 12, 31);
string dateStr = date.ToString("dd MMMM yyyy");

Console.WriteLine(dateStr); // Sortie: 31 décembre 2021
```

Plongeons plus profond:

La conversion de date en chaîne de caractères est une tâche fréquente dans la programmation et est supportée par de nombreuses langues, dont C#. Avant la sortie de C# en 2002, les programmeurs devaient utiliser des bibliothèques tierces pour effectuer cette conversion.

Une alternative à la conversion de date en chaîne de caractères est de stocker la date sous forme de valeur numérique, telle qu'un timestamp, qui représente le nombre de secondes écoulées depuis une date de référence. Cela peut être plus efficace en termes de performance, mais peut être moins lisible pour les utilisateurs.

Les détails techniques de la conversion de date en chaîne de caractères peuvent varier en fonction de la plateforme et de la culture de l'ordinateur sur lequel le code est exécuté. Cela peut entraîner des problèmes de compatibilité si le code est déployé sur différents ordinateurs ou dans différents pays.

Voir aussi:

- Documentation Microsoft sur la conversion de date en chaîne de caractères en C#: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring
- Différents formats de date et comment les utiliser en C#: https://www.c-sharpcorner.com/blogs/date-and-time-format-in-c-sharp-programming1
- D'autres langues prenant en charge la conversion de date en chaîne de caractères: Java, Python.