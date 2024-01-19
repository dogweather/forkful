---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La chaîne d'interpolation en C # permet d'insérer les valeurs des variables directement dans du texte en les entourant de crochets {} à l'intérieur d'une chaîne de texte préfixée par un signe dollar $. Les développeurs l'utilisent pour un affichage plus lisible et une meilleure maintenance du code.

## Comment faire :

Voici un exemple simple d'interpolation de chaîne
```C#
string nom = "John";
Console.WriteLine($"Bonjour, {nom} !"); // Sortie : Bonjour, John !
```
Il est également possible d'afficher le résultat de l'expression dans une chaîne interpolée.

```C#
int age = 25;
Console.WriteLine($"Dans cinq ans, vous aurez {age + 5} ans."); // Sortie : Dans cinq ans, vous aurez 30 ans.
```
## Plongée en Profondeur:

Historiquement, l'interpolation de chaîne est l'évolution des méthodes String.Format et des concaténations de chaînes pour rendre le code plus lisible et facile à entretenir.
```C#
// Ancienne méthode avec String.Format
Console.WriteLine(String.Format("Bonjour, {0} !", nom));
```
C# offre d'autres alternatives comme l'utilisation des concaténations de strings ou les fonctions `String.Format` ou `String.Concat` mais l'interpolation de chaîne est plus concise.

Les chaînes interpolées traitées par le compilateur C# comme un `String.Format` sous le capot pour donner un style plus modulaire et lisible.

## Voir Aussi:

- Documentation Microsoft sur l'interpolation de chaîne en C# : [String interpolation-C# Guide](https://docs.microsoft.com/fr-fr/dotnet/csharp/tutorials/string-interpolation)
- Introduction à l'interpolation de chaîne : [Raccourcis avec l'interpolation de chaîne en C#](https://www.infoworld.com/article/2989972/string-interpolation-in-c-6-0-a-beginners-guide.html) 
- Discussion de Stack Overflow sur l'interpolation de chaîne vs `String.Format`: [Is string interpolation in C# faster than string.format?](https://stackoverflow.com/questions/34737116/is-string-interpolation-in-c-sharp-faster-than-string-format)