---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Le fait de supprimer des caractères qui correspondent à un motif est une pratique courante en programmation. Ce procédé permet de nettoyer les données d'entrée ou de manipuler des chaînes de caractères en éliminant les motifs indésirables.

## Comment faire :
Voici un exemple de suppression de caractères correspondant à un motif en C#. Ici, nous supprimons toutes les occurrences du motif "abc" dans une chaîne de caractères.

```C#
using System;
using System.Text.RegularExpressions;

class Program {
    static void Main() {
        string pattern = "abc";
        string input = "123abc456abc";
        string output = Regex.Replace(input, pattern, "");

        Console.WriteLine(output); // Displays "123456"
    }
}
```
Dans cet exemple, la méthode `Regex.Replace` est utilisée pour remplacer toutes les occurrences du motif par une chaîne vide, ce qui revient à les supprimer.

## Plongée profonde

Historiquement, la suppression de caractères correspondant à un motif est une fonctionnalité qui a été introduite pour la première fois dans les langages de programmation tels que Perl et Python. En C#, nous utilisons l'API Regex (Expression régulière) pour effectuer cette tâche.

Une alternative à l'utilisation des expressions régulières est l'utilisation de la méthode `String.Replace`, qui est plus simple mais offre moins de flexibilité.

```C#
string input = "123abc456abc";
string output = input.Replace("abc", "");

Console.WriteLine(output); // Displays "123456"
```
Il est important de noter que la suppression de caractères est une opération qui ne modifie pas la chaîne originale car les chaînes en C# sont immuables. Au lieu de cela, une nouvelle chaîne est créée.

## Voir aussi

Pour plus d'informations sur les expressions régulières en C#, consultez ces liens :

- Documentation officielle Microsoft : [Regex Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex) 
- Un excellent tutoriel sur les expressions régulières : [Regular Expressions in C#](https://www.c-sharpcorner.com/article/regular-expressions-in-C-Sharp/) 
- Autres méthodes de la chaîne de caractères en C# : [String methods](https://www.w3schools.com/cs/cs_strings.php)