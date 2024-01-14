---
title:                "C#: Concaténation de chaînes de caractères"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes est une fonctionnalité courante en programmation qui vous permet de combiner plusieurs chaînes de caractères en une seule. Cela peut être utile pour créer des messages d'erreur personnalisés, des noms de fichiers dynamiques ou tout simplement pour améliorer la lisibilité de votre code en utilisant des variables pour les chaînes.

## Comment faire

La concaténation de chaînes en C# est très simple et peut être réalisée de plusieurs façons. Voici quelques exemples :

```C#
// Utiliser l'opérateur de concaténation "+"
string name = "Jean";
string greeting = "Bonjour " + name; // Output : Bonjour Jean

// Utiliser la méthode .Concat() de la classe String
string day = "Lundi";
string date = ". Aujourd'hui c'est le " .Concat(day); // Output : Aujourd'hui c'est le Lundi

// Utiliser la méthode .Join() de la classe String
string[] fruits = {"pomme", "banane", "orange"};
string result = String.Join(", ", fruits); // Output : pomme, banane, orange
```

Comme vous pouvez le constater, la concaténation de chaînes est assez simple à mettre en œuvre en utilisant les outils fournis par le langage C#.

## Plongée en profondeur

En réalité, la concaténation de chaînes en C# est plus complexe qu'il n'y paraît. Lorsque vous utilisez l'opérateur "+", il est important de noter que derrière le voile, il utilise en fait la méthode .Concat() de la classe String. Cela signifie que pour chaque utilisation de l'opérateur, une nouvelle chaîne est créée, ce qui peut avoir un impact sur les performances. Cela peut être évité en utilisant la classe StringBuilder, qui est spécifiquement conçue pour manipuler efficacement les chaînes de caractères.

De plus, il est important de comprendre que les chaînes en C# sont immuables, ce qui signifie qu'elles ne peuvent pas être modifiées une fois créées. Cela peut sembler contre-intuitif lors de l'utilisation de méthodes comme .Concat(), mais en réalité, elle renvoie une nouvelle chaîne contenant la concaténation et laisse les chaînes d'origine inchangées.

## Voir aussi

- [Documentation Microsoft sur la concaténation de chaînes en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/strings/#concatenation)
- [Tutoriel YouTube sur la concaténation de chaînes en C#](https://www.youtube.com/watch?v=KX1-YdxXDTO)
- [Article sur l'utilisation de la classe StringBuilder en C#](https://www.c-sharpcorner.com/UploadFile/fcdec9/string-vs-stringbuilder/)