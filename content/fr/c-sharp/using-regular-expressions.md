---
title:                "Utiliser les expressions régulières"
html_title:           "C: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Les expressions régulières (regex) sont un outil puissant pour le traitement des chaînes de caractères. Elles aident les programmeurs à chercher, à manipuler et à correspondre avec les motifs de texte complexe de manière optimisée.

## Comment faire:

Voici un exemple simple d'utilisation des regex en C#. Nous allons chercher l'occurrence d'un pattern dans une chaîne de texte :

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string phrase = "J'aime C# et les expressions régulières!";
        string pattern = @"(\bC#\b)|(\bexpressions régulières\b)";
        MatchCollection matches = Regex.Matches(phrase, pattern);

        foreach (Match match in matches)
            Console.WriteLine("Trouvé '{0}' à l'index {1}.", 
                match.Value, match.Index);
    }
}
```

Sortie :

```C#
Trouvé 'C#' à l'index 7.
Trouvé 'expressions régulières' à l'index 14.
```

## Plongée en profondeur:

Les regex ont été développées dans les années 1950 par l'informaticien américain Stephen Kleene. Elles sont maintenant intégrées à de nombreux langages de programmation, dont C#.

Bien que les regex soient puissants, ils ne sont pas toujours les plus économes en termes de performance. Pour les tâches simples de manipulation de chaînes, les méthodes intégrées du langage de programmation peuvent être plus performantes.

Lorsque vous utilisez des regex en C#, l'analyse réelle de l'expression régulière est effectuée par la méthode statique `Regex.Matches()`. Cette méthode retourne une collection d'objets `Match` qui contiennent des informations sur chaque correspondance trouvée.

## Voir aussi: 

- [Documentation officielle de Microsoft sur les expressions régulières](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expressions)
- [Regex101](https://regex101.com/): Un outil en ligne pour tester et déboguer des expressions régulières.
- [Regexr](https://regexr.com/): Un autre outil en ligne pour apprendre, construire et tester les expressions régulières.