---
title:                "C#: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi capitaliser une chaîne de caractères ?

Capitaliser une chaîne de caractères est une opération courante en programmation. Cela consiste à mettre la première lettre de chaque mot en majuscule et le reste en minuscule. Cette pratique est utile pour améliorer la lisibilité et la cohérence d'un texte, en particulier lorsqu'il s'agit d'afficher des informations à l'utilisateur.

## Comment le faire en C# ?

Il existe plusieurs façons de capitaliser une chaîne de caractères en C#. La méthode la plus simple consiste à utiliser la fonction intégrée ToTitleCase() de la classe TextInfo. Voici un exemple de code qui capitalise une chaîne saisie par l'utilisateur :

```C#
using System;
using System.Globalization;

namespace CapitalizeString
{
    class Program
    {
        static void Main(string[] args)
        {
            // Demande à l'utilisateur de saisir une chaîne de caractères
            Console.WriteLine("Entrez une chaîne de caractères : ");
            string input = Console.ReadLine();

            // Utilise la classe TextInfo pour capitaliser la chaîne
            TextInfo myTI = new CultureInfo("fr-FR", false).TextInfo;
            string capitalizedInput = myTI.ToTitleCase(input);

            // Affiche la chaîne capitalisée
            Console.WriteLine("Chaîne capitalisée : " + capitalizedInput);
            Console.ReadLine();
        }
    }
}
```

**Exemple d'entrée et de sortie :**
```
Entrez une chaîne de caractères : bonjour tout le monde
Chaîne capitalisée : Bonjour Tout Le Monde
```

Il est également possible de capitaliser une chaîne en utilisant la fonction Substring() pour extraire la première lettre et la mettre en majuscule, puis en ajoutant le reste de la chaîne. Voici un exemple :

```C#
string input = "hello world";
string capitalizedInput = input.Substring(0, 1).ToUpper() + input.Substring(1).ToLower();
Console.WriteLine(capitalizedInput);
```

**Sortie :** Hello world

## Approfondissement

En plus des méthodes mentionnées ci-dessus, il est également possible de capitaliser une chaîne en utilisant des expressions régulières ou en créant une fonction personnalisée. Cependant, il est important de noter que ces méthodes peuvent avoir un impact sur les performances de votre code en raison de leur complexité.

Il est également important de prendre en compte la langue et la culture lors de la capitalisation d'une chaîne de caractères. Par exemple, en français, les règles d'accents et de majuscules peuvent varier en fonction du contexte. Il est donc important de bien comprendre les besoins et les spécifications de votre application avant de choisir la méthode la plus appropriée pour capitaliser une chaîne.

## Voir aussi

- Documentation Microsoft sur la fonction ToTitleCase() : https://msdn.microsoft.com/fr-fr/library/system.globalization.textinfo.totitlecase(v=vs.110).aspx
- Guide de référence sur les expressions régulières en C# : https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference
- Tutoriel sur la création de fonctions personnalisées en C# : https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/classes-and-structs/how-to-define-and-use-custom-numeric-format-providers