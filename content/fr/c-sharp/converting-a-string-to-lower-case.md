---
title:    "C#: Convertir une chaîne de caractères en minuscules"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est courant d'avoir besoin de manipuler du texte. Parfois, nous avons besoin de modifier le format du texte pour des raisons spécifiques. L'une de ces modifications peut être de convertir une chaîne de caractères en lettres minuscules. Dans cet article, nous allons explorer pourquoi cela est utile et comment le faire en utilisant le langage de programmation C#.

## Comment faire 

Pour convertir une chaîne de caractères en lettres minuscules en utilisant C#, nous allons utiliser la méthode `ToLower()` de la classe `String`. Cette méthode retourne une nouvelle chaîne de caractères avec toutes les lettres en minuscules. Voyons un exemple concret :

```C#
string text = "EXEMPLE DE TEXTE";
string convertedText = text.ToLower();

Console.WriteLine(convertedText);
```

La sortie de ce code sera "exemple de texte". Nous avons créé une nouvelle chaîne de caractères en assignant le résultat de la méthode `ToLower()` à une variable et en l'affichant à l'aide de `Console.WriteLine()`.

Nous pouvons également utiliser cette méthode pour modifier directement une chaîne de caractères existante :

```C#
string text = "EXEMPLE DE TEXTE";
text = text.ToLower();

Console.WriteLine(text);
```

Dans ce cas, la sortie sera également "exemple de texte". Cela peut être utile si nous voulons remplacer le texte d'origine par le texte en minuscules.

## Plongée en profondeur

Il est important de noter que la méthode `ToLower()` utilise les règles de casse actuelles du système d'exploitation en cours d'exécution. Par exemple, si vous utilisez Windows, la conversion sera différente de celle sur un système d'exploitation basé sur Linux.

De plus, cette méthode ne prend pas en compte les caractères spéciaux ou accents. Par exemple, le caractère "É" ne sera pas converti en "é". Si vous avez besoin d'une conversion plus précise, vous pouvez utiliser la méthode `ToLowerInvariant()` qui utilise les règles de casse invariantes, peu importe le système d'exploitation.

## Voir aussi

- [Documentation officielle de la méthode `ToLower()` en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.tolower?view=net-5.0)
- [Différence entre `ToLower()` et `ToLowerInvariant()` en C#](https://stackoverflow.com/questions/450472/string-tolower-vs-string-tolowerinvariant)
- [Guide complet sur la manipulation des chaînes de caractères en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/strings/)