---
title:                "Agrégation de chaînes de caractères"
html_title:           "C#: Agrégation de chaînes de caractères"
simple_title:         "Agrégation de chaînes de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes de caractères est un outil utile pour combiner différentes valeurs ou morceaux de texte en une seule chaîne. Cela peut être particulièrement utile lors de la création de messages, de rapports ou d'interfaces utilisateur dynamiques. Cela évite également la duplication de code en utilisant des chaînes pré-formatées.

## Comment faire

Lors de la concaténation de chaînes de caractères en C#, vous pouvez utiliser l'opérateur "+" ou la méthode Concat(). Voici un exemple de chaque méthode :

```C#
// Utiliser l'opérateur "+"
string name = "John";
string greeting = "Bonjour " + name + "!"; // résultat : "Bonjour John!"

// Utiliser la méthode Concat()
string firstName = "Jane";
string lastName = "Doe";
string fullName = string.Concat(firstName, " ", lastName); // résultat : "Jane Doe"
```

Il est également possible de concaténer un grand nombre de chaînes de caractères en utilisant la classe StringBuilder. Cela peut améliorer les performances et la gestion de la mémoire. Voici un exemple d'utilisation de StringBuilder :

```C#
StringBuilder sb = new StringBuilder();

sb.Append("J'aime");
sb.Append(" ");
sb.Append("coder");
sb.Append(" ");
sb.Append("en C#!");

string result = sb.ToString(); // résultat : "J'aime coder en C#!"
```

## Plongée en profondeur

Il est important de noter que la concaténation de chaînes de caractères en C# crée une nouvelle chaîne à chaque fois qu'elle est utilisée. Cela peut entraîner des problèmes de performances et de gestion de la mémoire lorsqu'il y a un grand nombre de chaînes à concaténer.

Il est également possible d'utiliser la méthode Join() pour concaténer plusieurs chaînes de caractères avec un délimiteur spécifique. Voici un exemple :

```C#
string[] words = { "Hello", "world", "!" };
string result = string.Join(" ", words); // résultat : "Hello world !"
```

## Voir aussi

- [Documentation Microsoft sur la concaténation de chaînes en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/how-to/compare-strings)
- [Blog de Scott Hanselman sur l'utilisation de StringBuilder pour de meilleures performances](https://www.hanselman.com/blog/DoTheStringBuilderThing.aspx)
- [Article de Codeburst expliquant la différence entre l'opérateur "+" et la méthode Concat() pour concaténer des chaînes en C#](https://codeburst.io/string-concatenation-in-c-under-the-hood-b8191d469025)