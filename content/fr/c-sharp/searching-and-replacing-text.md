---
title:    "C#: Recherche et remplacement de texte"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Pourquoi utiliser la recherche et le remplacement de texte en C#?

La recherche et le remplacement de texte sont des techniques couramment utilisées dans la programmation en C#, permettant de modifier rapidement et efficacement le contenu d'une chaîne de caractères. Cette fonctionnalité est particulièrement utile pour réaliser des modifications en masse ou pour trouver et remplacer des mots spécifiques dans un texte.

## Comment le faire en C#?

Pour utiliser la recherche et le remplacement de texte en C#, il existe plusieurs méthodes. La plus simple est d'utiliser la méthode `Replace()` de la classe `String`. Cette méthode prend en paramètres la chaîne de caractères à remplacer, ainsi que la nouvelle valeur à lui affecter. Voici un exemple dans un code block ```C#
string text = "Bonjour le monde!";
string newText = text.Replace("Bonjour", "Hello");

Console.WriteLine(newText);
// Output: Hello le monde!
```

Il est également possible d'utiliser des expressions régulières avec la classe `Regex` pour réaliser des recherches et remplacements plus complexes. Par exemple, si nous voulons remplacer toutes les occurrences de lettres majuscules par des lettres minuscules dans un texte, nous pouvons utiliser le code suivant:
````C#
string text = "C# Est Un Langage De Programmation";
string newText = Regex.Replace(text, "[A-Z]", match => match.Value.ToLower());

Console.WriteLine(newText);
// Output: c# est un langage de programmation
````

## Plongée en profondeur

La méthode `Replace()` de la classe `String` est pratique pour des remplacements simples, mais elle peut parfois être limitée pour des cas plus complexes. C'est là que les expressions régulières entrent en jeu. En utilisant des expressions régulières, il est possible de trouver et remplacer des motifs précis dans une chaîne de caractères. Par exemple, si nous voulons remplacer tous les chiffres dans une chaîne par un symbole "*", nous pouvons utiliser le code suivant:
````C#
string text = "Bonjour 123 le monde 456 !";
string newText = Regex.Replace(text, "[0-9]", "*");

Console.WriteLine(newText);
// Output: Bonjour *** le monde *** !
````

En utilisant des expressions régulières, les possibilités de recherche et de remplacement de texte sont infinies en C#. Cela peut être très utile pour des tâches telles que la validation de données utilisateur ou la manipulation de fichiers.

# Voir aussi

- [Documentation Microsoft sur la méthode `String.Replace()`](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.replace)
- [Documentation Microsoft sur la classe `Regex`](https://docs.microsoft.com/fr-fr/dotnet/api/system.text.regularexpressions.regex)