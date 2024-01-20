---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concaténation de Chaînes en C# : Un Guide Pratique et Simple

## Quoi & Pourquoi ?
La concaténation de chaînes est le processus d'ajouter deux ou plusieurs chaînes bout à bout. Ce geste est courant pour les programmeurs quand ils veulent combiner des informations textuelles.

## Comment faire :
Voici les exemples courants de la concaténation de chaînes en C#.

```C#
string salut = "Bonjour";
string nom = "Maria";
string message = salut + ", " + nom; // Concaténation en utilisant l'opérateur +
Console.WriteLine(message); 
// Output: Bonjour, Maria
```

Ou vous pouvez le faire en utilisant la méthode `String.Concat()`.

```C#
string salut = "Bonjour";
string nom = "Maria";
string message = String.Concat(salut, ", ", nom);
Console.WriteLine(message); 
// Output: Bonjour, Maria
```

## Plongée Profonde :

1. **Contexte historique :** La concaténation de chaînes en C# a été simplifiée au fil des versions. L'utilisation de `+` et `String.Concat()` remonte aux premières versions de C#, tandis que des fonctions plus avancées, comme `StringBuilder`, ont été introduites plus tard.

2. **Alternatives :** Une alternative populaire à la concaténation de chaînes est `StringBuilder`. Elle est particulièrement utile lorsqu'on manipule une grande quantité de chaînes, car elle est plus efficace en termes de performances.

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Bonjour");
sb.Append(", ");
sb.Append("Maria");
Console.WriteLine(sb.ToString()); 
// Output: Bonjour, Maria
```

3. **Détails d'implémentation :** Lorsque vous concaténez des chaînes avec `+` ou `Concat()`, C# crée une nouvelle chaîne et y copie les chaînes originales. Cependant, chaque ajout génère une nouvelle chaîne, ce qui peut être inefficace pour les grandes quantités de données. `StringBuilder` résout ce problème en modifiant la chaîne existante, ce qui économise de la mémoire.

## Voir Aussi :
Voici quelques références qui pourraient vous intéresser pour approfondir vos connaissances sur la concaténation de chaînes en C#.


1. Documentation .NET sur `String.Concat`: [Lien](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.concat)
2. Documentation .NET sur `StringBuilder`: [Lien](https://docs.microsoft.com/fr-fr/dotnet/api/system.text.stringbuilder)