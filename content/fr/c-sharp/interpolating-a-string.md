---
title:                "Interpoler une chaîne de caractères"
html_title:           "C#: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

Qu'est-ce que l'interpolation de chaîne en C# et pourquoi les programmeurs le font-ils ?

L'interpolation de chaîne en C# est un moyen pratique d'insérer des variables dans une chaîne de caractères sans avoir à utiliser des concaténations ou des conversions de types. Elle utilise la notation $ {} pour spécifier les variables à insérer dans une chaîne, ce qui rend le code plus lisible et plus facile à gérer. Les programmeurs choisissent souvent d'utiliser l'interpolation de chaîne pour améliorer l'efficacité et la lisibilité de leur code.

Comment faire :

```C#
// Exemple de base
string nom = "Marie";
string message = $"Bonjour {nom}, comment ça va ?";
Console.WriteLine(message);
// Output : Bonjour Marie, comment ça va ?

// Exemple avec des opérations
int x = 5;
int y = 10;
string resultat = $"La somme de {x} et {y} est égale à {x + y}.";
Console.WriteLine(resultat);
// Output : La somme de 5 et 10 est égale à 15.
```

Plongée en profondeur :

L'interpolation de chaîne en C# a été introduite avec la version 6 du langage en 2015. Elle a été inspirée par une fonctionnalité similaire dans le langage de programmation Ruby. Avant cela, les programmeurs utilisaient principalement la méthode .Format () pour insérer des variables dans une chaîne. Bien que cette méthode soit toujours prisée par certains programmeurs, l'interpolation de chaîne est généralement considérée comme un moyen plus simple et plus efficace d'insérer des variables dans une chaîne en C#.

Alternativement, les développeurs peuvent utiliser la concaténation de chaîne (+) ou des fichiers de ressources pour incorporer des variables dans une chaîne. Toutefois, ces méthodes peuvent être fastidieuses et moins pratiques que l'interpolation de chaîne.

L'interpolation de chaîne est implémentée en utilisant la méthode String.Format () avec des paramètres spéciaux qui contiennent des valeurs à insérer dans la chaîne. La compilation se fait à l'aide des expressions lambda, ce qui garantit une bonne performance lorsque vous utilisez l'interpolation de chaîne dans votre code.

A voir aussi :

Pour en savoir plus sur l'interpolation de chaîne et d'autres fonctionnalités de C#, vous pouvez consulter la documentation officielle de Microsoft pour le langage C# : https://docs.microsoft.com/dotnet/csharp/. Vous pouvez également trouver des tutoriels et des ressources utiles sur des sites de programmation populaires tels que Stack Overflow et Codecademy.