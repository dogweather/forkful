---
title:                "C#: Extraction de sous-chaînes"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une opération couramment utilisée en programmation pour récupérer une partie spécifique d'une chaîne de caractères. Cela peut être utile pour de nombreuses tâches, telles que la manipulation de données ou la création de formats de chaînes personnalisés.

## Comment Faire

Pour extraire une sous-chaîne en C#, vous pouvez utiliser la méthode `Substring` sur un objet de type `string`. Cette méthode prend deux paramètres : l'index de départ et la longueur de la sous-chaîne souhaitée.

```C#
string chaine = "La programmation en C# est amusante !";
string sousChaine = chaine.Substring(5, 11);

Console.WriteLine(sousChaine); // affiche "programmation"
```

La méthode `Substring` commence à compter à partir de 0, donc l'index de départ pour "programmation" est 5 (car il s'agit du sixième caractère dans la chaîne). Si vous ne spécifiez pas la longueur de la sous-chaîne, elle sera automatiquement définie comme la fin de la chaîne d'origine.

Vous pouvez également utiliser des index négatifs pour compter à partir de la fin de la chaîne, par exemple :

```C#
string chaine = "La programmation en C# est amusante !";
string sousChaine = chaine.Substring(5, chaine.Length - 28);

Console.WriteLine(sousChaine); // affiche "programmation"
```

Dans cet exemple, nous utilisons la propriété `Length` pour déterminer la longueur de la chaîne d'origine et nous soustrayons la longueur de la sous-chaîne (28 caractères) pour obtenir la longueur totale de la sous-chaîne.

## Plongée Profonde

Il est également possible d'extraire des sous-chaînes à partir d'autres types de données, tels que des tableaux de caractères. Dans ce cas, vous pouvez utiliser la méthode `Subarray` pour récupérer une partie spécifique du tableau. Par exemple :

```C#
char[] lettres = {'a', 'b', 'c', 'd', 'e', 'f'};
char[] sousLettres = lettres.Subarray(2, 3);

foreach (char lettre in sousLettres)
{
    Console.Write(lettre + " "); // affiche "c d e"
}
```

La méthode `Subarray` utilise également les mêmes paramètres que `Substring` : l'index de départ et la longueur de la sous-chaîne.

## Voir Aussi

- [Documentation Microsoft sur la méthode Substring](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.substring)
- [Documentation Microsoft sur la méthode Subarray](https://docs.microsoft.com/fr-fr/dotnet/api/system.array.subarray)
- [Article sur les manipulations de chaînes en C#](https://www.tutorialsteacher.com/csharp/csharp-string)