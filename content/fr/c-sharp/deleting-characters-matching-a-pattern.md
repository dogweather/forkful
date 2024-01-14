---
title:    "C#: Suppression des caractères correspondant à un motif"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile de supprimer des caractères correspondant à un motif dans un programme C#. Cela peut aider à nettoyer des données ou à simplifier les recherches de texte dans une application.

## Comment faire

Il existe plusieurs façons de supprimer des caractères correspondant à un motif en utilisant C#. La méthode la plus simple est d'utiliser la classe Regex et sa fonction Replace, qui permet de remplacer des morceaux de chaîne par une chaîne vide.

````C#
string text = "Bonjour, je suis un exemple de texte contenant des chiffres : 123456";
string pattern = "[0-9]"; // sélectionne tous les chiffres de 0 à 9
string result = Regex.Replace(text, pattern, "");
Console.WriteLine(result);
// Output : "Bonjour, je suis un exemple de texte contenant des chiffres : "
````

Une autre méthode consiste à utiliser la méthode Remove de la classe StringBuilder, ce qui peut être plus efficace lorsque l'on manipule de grandes chaînes de caractères.

````C#
string text = "Supprimer ces lettres : abcdefg";
string pattern = "[a-g]"; // sélectionne les lettres de a à g
StringBuilder sb = new StringBuilder(text);
Regex r = new Regex(pattern);
foreach (Match match in r.Matches(text))
{
    sb.Remove(match.Index, 1); // supprime la lettre correspondant au match
}
Console.WriteLine(sb);
// Output : "Supprimer ces lettres : "
````

## Profondeur d'analyse

Lors de la suppression de caractères correspondant à un motif, il est important de comprendre clairement quel motif est recherché et comment il est sélectionné. Les expressions régulières permettent de sélectionner des motifs complexes, tandis que les méthodes de manipulation de chaînes plus basiques peuvent être plus efficaces pour des remplacements simples.

## Voir aussi

- [L'utilisation des expressions régulières en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Manipuler des chaînes avec C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/strings/)
- [La classe Regex en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)