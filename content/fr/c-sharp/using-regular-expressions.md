---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières sont des motifs utilisés pour trouver des correspondances de texte complexes. Les programmeurs s'en servent pour valider, rechercher ou manipuler des chaînes de caractères, comme pour la validation d'emails ou la recherche de mots-clés.

## Comment faire :
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = "Mon email est example@email.com.";
        string pattern = @"\b[\w\.-]+@[\w\.-]+\.\w{2,6}\b";
        
        Match match = Regex.Match(input, pattern);
        if (match.Success)
        {
            Console.WriteLine("Email trouvé : " + match.Value);
        }
    }
}
```
Sortie : `Email trouvé : example@email.com.`

## Exploration Approfondie
Les expressions régulières sont nées dans les années 1950 de la théorie mathématique des langages formels. Aujourd'hui, il existe d'autres moyens de travailler avec les chaînes de caractères, comme les méthodes de chaînes intégrées en C#, mais elles n'offrent pas la même flexibilité ou puissance. L'implémentation C# des regex se trouve dans l'espace de noms `System.Text.RegularExpressions` et utilise l'automate fini sous-jacent pour analyser les expressions.

## Voir Aussi
- Documentation Microsoft sur les expressions régulières en C# : https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference
- Livre "Mastering Regular Expressions" pour une compréhension approfondie : https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/
- Site Web pour tester et affiner vos expressions régulières : https://regex101.com/
