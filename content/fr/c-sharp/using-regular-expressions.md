---
title:    "C#: Utiliser les expressions régulières"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi utiliser les expressions régulières en programmation C#?

Les expressions régulières sont un outil très puissant qui permettent de rechercher et de manipuler du texte en utilisant des motifs spécifiques. En utilisant des expressions régulières, vous pouvez créer des règles de recherche et de remplacement très précises pour traiter vos données. Cela peut être utile dans de nombreuses situations différentes, comme par exemple dans le traitement de formulaires en ligne, la vérification de la validité des adresses e-mail ou la recherche de mots-clés dans un document.

## Comment utiliser les expressions régulières en C#?

Pour utiliser les expressions régulières en C#, vous devez d'abord importer l'espace de noms `System.Text.RegularExpressions`. Ce dernier contient les classes et les méthodes nécessaires pour travailler avec les expressions régulières.

Voici un exemple de code simple qui recherche toutes les occurrences de la lettre "e" dans une chaîne de caractères en utilisant l'expression régulière `e`.

```C#
using System;
using System.Text.RegularExpressions;

namespace ExempleRegex
{
    class Program
    {
        static void Main(string[] args)
        {
            string phrase = "Bonjour tout le monde!";
            string pattern = "e";
            MatchCollection matches = Regex.Matches(phrase, pattern);

            foreach (Match match in matches)
            {
                Console.WriteLine("Occurrence trouvée: " + match.Value);
            }
        }
    }
}
```

L'output de ce code sera:

```
Occurrence trouvée: e
Occurrence trouvée: e
Occurrence trouvée: e
```

Vous pouvez également utiliser des métacaractères pour effectuer des recherches plus précises. Les métacaractères sont des caractères spéciaux qui représentent des groupes de caractères.

Voici un exemple de code qui vérifie si un numéro de téléphone est valide en utilisant l'expression régulière `\d{3}-\d{3}-\d{4}`, qui correspond au format des numéros de téléphone américains:

```C#
using System;
using System.Text.RegularExpressions;

namespace ExempleRegex
{
    class Program
    {
        static void Main(string[] args)
        {
            string numero = "123-456-7890";
            string pattern = @"\d{3}-\d{3}-\d{4}";
            Match match = Regex.Match(numero, pattern);

            if (match.Success)
            {
                Console.WriteLine("Le numéro de téléphone est valide.");
            }
            else
            {
                Console.WriteLine("Le numéro de téléphone n'est pas valide.");
            }
        }
    }
}
```

## Approfondissement: Utilisation avancée des expressions régulières en C#

Les expressions régulières en C# offrent de nombreuses fonctionnalités avancées telles que les groupes de capture, les modificateurs et les expressions régulières récursives. L'utilisation de ces fonctionnalités peut être très utile dans des cas plus complexes de recherche et de manipulation de texte.

Par exemple, vous pouvez utiliser un groupe de capture pour extraire une partie spécifique d'une chaîne de caractères. Voici un exemple de code qui extrait les noms de domaine des adresses e-mail en utilisant l'expression régulière `@(\w+\.\w+)`:

```C#
using System;
using System.Text.RegularExpressions;

namespace ExempleRegex
{
    class Program
    {
        static void Main(string[] args)
        {
            string email = "john.doe@example.com";
            string pattern = "@(\w+\.\w+)";
            Match match = Regex.Match(email, pattern);

            if (match.Success)
            {
                Console.WriteLine("Domaine: " + match.Groups[1].Value);
            }
        }
    }
}
```

L'output de ce code sera:

```
Domaine: example.com
```

Pour en savoir plus sur les expressions régulières en C#, consultez la documentation officielle de Microsoft sur les expressions régulières en C# ou divers tutoriels en ligne.

## Voir aussi
- [Documentation Microsoft sur les expressions régulières en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expressions)
- [Tutoriel sur les expressions régulières en C#](https://www.bogotobogo.com/cplusplus/regexes.php)
- [Référence rapide des expressions régulières en C#](https://www.rexegg.com/regex-quickstart.html)