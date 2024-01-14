---
title:    "C#: Convertir une chaîne en minuscules"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
La conversion d'une chaîne de caractères en minuscules peut être utile dans de nombreuses situations, notamment pour faciliter la comparaison de chaînes ou pour l'affichage de données dans un format plus lisible.

## Comment faire
Voici un exemple de code en C# pour convertir une chaîne de caractères en minuscules :

```C#
string originalString = "Bonjour tout le monde!";
string lowerCaseString = originalString.ToLower();
Console.WriteLine(lowerCaseString);
```

La sortie de ce code sera "bonjour tout le monde!" car toutes les lettres ont été converties en minuscules.

## Plongée en profondeur
Lorsque vous utilisez la méthode .ToLower() en C#, il y a quelques points à prendre en compte. Tout d'abord, cette méthode est sensible à la culture, ce qui signifie que la manière dont elle convertit les lettres dépendra de la culture du système sur lequel elle est exécutée. Par exemple, en français, la lettre "I" sera convertie en "i" alors qu'en turc, elle sera convertie en "ı".

De plus, la méthode .ToLower() ne convertira que les lettres et ignora les caractères spéciaux tels que les accents, les cédilles ou les trémas. Par exemple, la chaîne "École" sera convertie en "école".

Enfin, il est important de noter que la méthode .ToLower() ne modifie pas la chaîne d'origine mais renvoie une nouvelle chaîne convertie en minuscules.

## Voir aussi
- [Méthode String.ToLower() en C# (Microsoft Documentation)](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.tolower)
- [Méthodes de comparaison de chaînes en C# (Blog BetterProgramming)](https://betterprogramming.pub/string-comparison-methods-in-c-77da3d5080b8)
- [Guide complet pour les méthodes ToLower() et ToUpper() en C# (Blog DaedTech)](https://daedtech.com/string-case-comparison-methods-in-csharp/)