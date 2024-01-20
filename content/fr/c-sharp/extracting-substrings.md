---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

L'extraction de sous-chaînes est l'action de prendre une partie d'une chaîne de caractères. Les programmeurs le font pour manipuler et utiliser des données à l'intérieur d'une chaîne plus grande.

## Comment faire :

Voici un exemple d'extraction d'une sous-chaîne en utilisant C# :

```C#
string phrase = "Bonjour tout le monde";
string sousChaine = phrase.Substring(8, 4); // Résultat: "tout"
Console.WriteLine(sousChaine);
```

Dans cet exemple, 'Substring' est utilisé pour extraire le mot entre la position 8 et la position 12 de la phrase, et le résultat est affiché dans la console.

## Deep Dive

Historiquement, l'extraction de sous-chaînes existe depuis les premiers jours de la programmation pour travailler avec des chaînes de caractères. En C#, la méthode 'Substring' est fournie pour cette tâche, mais vous pouvez également utiliser les méthodes 'Indexof' et 'LastIndexOf' pour localiser la position d'un caractère ou d'une sous-chaîne dans une chaîne.

Il existe des alternatives à 'Substring', comme 'Split' si vous voulez diviser une chaîne en plusieurs parties ou 'Replace' pour changer certains éléments de la chaîne. L'option la plus appropriée dépend du problème que vous voulez résoudre.

L'implémentation de la méthode 'Substring' est assez simple. Elle utilise le tableau de caractères sous-jacent de la chaîne, et renvoie une nouvelle chaîne qui commence à l'index spécifié et a une longueur spécifiée.

## Voir Aussi

Pour plus d'informations sur l'extraction de sous-chaînes et les chaînes de caractères en C#, consultez les liens suivants:

- Documentation Microsoft sur les chaînes de caractères en C#: https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/strings/
- Guide de programmation C# sur les sous-chaînes: https://www.tutorialsteacher.com/csharp/csharp-string-substring
- Les méthodes de chaîne en C#: https://www.codingame.com/playgrounds/27940/les-methodes-de-chaine-en-c