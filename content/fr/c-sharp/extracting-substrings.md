---
title:    "C#: Extraction de sous-chaînes"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une technique utile dans de nombreuses situations de programmation en C#. Elle permet de récupérer une partie spécifique d'une chaîne de caractères, ce qui peut être très pratique lors de la manipulation de données ou de la création de conditions ou de boucles.

## Comment faire

Pour extraire une sous-chaîne en utilisant C#, vous pouvez utiliser la méthode `Substring()` de la classe `String`. Cette méthode prend en paramètre l'indice de départ et la longueur de la sous-chaîne à extraire.

Voici un exemple de code:

```C#
string phrase = "Bonjour le monde";
string sousChaine = phrase.Substring(8, 4);
Console.WriteLine(sousChaine);
```

Cet exemple va extraire la sous-chaîne "le" à partir de la position 8 (comptée à partir de zéro) dans la chaîne "Bonjour le monde". Le résultat affiché sera donc "le".

Vous pouvez également utiliser la méthode `Substring()` pour extraire une sous-chaîne à partir de la fin de la chaîne. Pour cela, il suffit de spécifier un indice de départ négatif, qui sera compté à partir de la fin de la chaîne.

Par exemple:

```C#
string phrase = "Salut tout le monde";
string sousChaine = phrase.Substring(12);
Console.WriteLine(sousChaine);
```

Le résultat affiché sera "monde" car l'indice 12 correspond à la position du "m" dans la chaîne.

## Plongée en profondeur

En plus de la méthode `Substring()`, il existe d'autres façons plus avancées d'extraire des sous-chaînes en utilisant C#. Par exemple, vous pouvez utiliser la classe `Regex` pour extraire des sous-chaînes en fonction d'un motif de correspondance.

De plus, vous pouvez également combiner l'extraction de sous-chaînes avec d'autres méthodes et fonctions de manipulation de chaînes afin de réaliser des tâches plus complexes.

## Voir aussi

- [Documentation officielle de la méthode Substring en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.substring)
- [Tutoriel sur l'utilisation des expressions régulières en C#](https://www.freecodecamp.org/news/an-easy-guide-to-understanding-regex-c524805a6988/) (en anglais)
- [Autres méthodes de manipulation de chaînes en C#](https://www.tutorialsteacher.com/csharp/csharp-string-class)