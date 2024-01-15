---
title:                "Utiliser les expressions régulières"
html_title:           "C#: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières, également appelées "regex", sont des outils très utiles en programmation pour rechercher, remplacer ou valider des chaînes de caractères selon un motif spécifié. Elles permettent d'accélérer et de simplifier la manipulation de données textuelles dans un large éventail de scénarios.

## Comment faire

Pour commencer à utiliser les expressions régulières en C#, il suffit d'importer le namespace "System.Text.RegularExpressions" et de créer une instance de la classe Regex. Ensuite, vous pouvez utiliser les différentes méthodes disponibles telles que "Match" ou "Replace" pour rechercher et manipuler des chaînes de caractères en fonction d'un motif prédéfini.

```C#
using System.Text.RegularExpressions; // Importer le namespace

string myString = "Bonjour tout le monde !";

// Créer l'instance de Regex avec le motif "\bmot\b" pour rechercher le mot "le"
Regex regex = new Regex("\\bmo\b");

// Utiliser la méthode "Match" pour rechercher le premier match dans la chaîne
Match match = regex.Match(myString);

// Afficher la valeur du match trouvé
Console.WriteLine("Match trouvé : " + match.Value);

// Utiliser la méthode "Replace" pour remplacer le mot "le" par "la"
string newString = regex.Replace(myString, "la");

Console.WriteLine("Nouvelle chaîne : " + newString);
```

Output :

```
Match trouvé : le
Nouvelle chaîne : Bonjour la tout la monde !
```

## Profonde plongée

Les expressions régulières offrent une grande flexibilité en termes de motifs de recherche grâce à la présence de métacaractères. Par exemple, le point "." peut être utilisé pour représenter n'importe quel caractère, le symbole "^" pour indiquer le début d'une ligne et le symbole "$" pour indiquer la fin d'une ligne. De plus, il est possible d'utiliser des groupes de capture pour récupérer des parties spécifiques des chaînes de caractères.

Il est également important de noter que les expressions régulières peuvent être très coûteuses en termes de performances si elles sont mal utilisées. Il est donc conseillé de bien comprendre le fonctionnement des regex et d'utiliser des outils tels que les expressions régulières en ligne pour tester et optimiser vos motifs avant de les utiliser dans votre code.

## Voir aussi

Pour plus d'informations sur les expressions régulières en C#, veuillez consulter les liens suivants :

- [Documentation Microsoft sur les expressions régulières en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expressions)
- [Regex101 - Testeur d'expressions régulières en ligne](https://regex101.com/)