---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Découvrir la longueur d'une chaîne en C#

## Quoi & Pourquoi?
La détermination de la longueur d'une chaîne correspond à compter le nombre de caractères qu'elle contient. C'est une tâche courante grâce à laquelle les programmeurs peuvent manipuler, segmenter et analyser des textes.

## Comment faire:
Voici un exemple rapide de la façon dont vous pouvez trouver la longueur d'une chaîne en C#.

```C#
using System;

public class Program
{
    public static void Main()
    {
        string exemple = "Longueur";
        Console.WriteLine(exemple.Length);
    }
}
```
L'exécution de ce script générera la sortie "8", car la chaîne "Longueur" est composée de 8 caractères.

## Plongée profonde

1. Contexte historique: Depuis l'introduction du C#, la propriété `Length` est la méthode standard pour déterminer la longueur d’une chaîne. C'est une approche rapide et efficace qui a peu de chances de disparaître.

2. Alternatives: Bien que `Length` soit la méthode prédominante, vous pouvez également utiliser la méthode `Count()` pour trouver la longueur de votre chaîne. Toutefois, cette dernière nécessite l'utilisation du namespace `System.Linq`.

```C#
using System;
using System.Linq;

public class Program
{
    public static void Main()
    {
        string exemple = "Longueur";
        Console.WriteLine(exemple.Count());
    }
}
```

3. Détails de mise en œuvre: En interne, la propriété `Length` renvoie simplement le nombre d'éléments dans le tableau de caractères qui stocke votre chaîne. C'est donc une opération très rapide.

## Voir Aussi:
- Documentation sur la propriété `Length` sur Microsoft Docs: [String.Length Propriété](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.length?view=net-5.0)
- Documentation sur `Count()` sur Microsoft Docs: [Enumerable.Count Méthode](https://docs.microsoft.com/fr-fr/dotnet/api/system.linq.enumerable.count?view=net-5.0)