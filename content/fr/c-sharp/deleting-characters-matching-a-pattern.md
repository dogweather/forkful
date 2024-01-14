---
title:    "C#: Supprimer les caractères correspondant à un motif"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi supprimer des caractères correspondant à un modèle?

Supprimer des caractères correspondant à un modèle peut être utile dans différentes situations en programmation. Par exemple, cela peut être nécessaire pour nettoyer des données ou pour simplifier des chaînes de caractères avant de les utiliser dans d'autres opérations. En général, cette opération permet de mieux gérer et manipuler les données dans un projet de programmation.

## Comment faire?

Voici un exemple de code en C# qui montre comment supprimer des caractères correspondant à un modèle :

```C#
string originalString = "ab123cd456ef";
string pattern = @"[a-z]|\d+";
string result = Regex.Replace(originalString, pattern, "");
Console.WriteLine(result); // Résultat : 123456
```

Dans cet exemple, nous utilisons la classe Regex pour rechercher et remplacer tous les caractères correspondant au modèle spécifié. La variable "originalString" contient la chaîne de caractères initiale, le modèle dans la variable "pattern" correspond à n'importe quel caractère alphabétique ou à un nombre d'un ou plusieurs chiffres. Le résultat final est stocké dans la variable "result" et affiché dans la console avec la méthode WriteLine.

Il est important de noter que la classe Regex nécessite l'utilisation de l'espace de noms "System.Text.RegularExpressions" pour fonctionner correctement. De plus, le modèle peut être modifié et adapté en fonction de vos besoins.

## Plongeons plus en profondeur

En plus des exemples de code, il est également important de comprendre ce qui se passe en coulisse lors de la suppression de caractères correspondant à un modèle. La classe Regex utilise des expressions régulières (ou RegEx) pour identifier et manipuler les données. Les expressions régulières sont des chaînes de caractères spéciales qui définissent un modèle à rechercher dans une autre chaîne de caractères.

Par exemple, dans notre code précédent, le modèle [a-z]|\d+ signifie :
- [a-z] : correspond à n'importe quel caractère alphabétique
- \d+ : correspond à un ou plusieurs chiffres

En combinant ces deux modèles avec le symbole "ou" (|), nous indiquons à la classe Regex de rechercher à la fois les caractères alphabétiques et les chiffres, ce qui entraîne la suppression de ces caractères dans le résultat final.

## Voir aussi

Pour en savoir plus sur les expressions régulières et l'utilisation de la classe Regex en C#, voici quelques liens utiles :
- [Documentation officielle de Microsoft sur les expressions régulières en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Tutoriel Codecademy sur les expressions régulières en C#](https://www.codecademy.com/fr/learn/learn-regular-expressions/modules/learn-regular-expressions-net/cheatsheet)
- [Tutoriel vidéo sur YouTube sur l'utilisation de la classe Regex en C#](https://www.youtube.com/watch?v=EnF1A4R8N6Y)