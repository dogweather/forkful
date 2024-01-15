---
title:                "Conversion d'une chaîne en minuscules"
html_title:           "C#: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est souvent nécessaire de convertir une chaîne de caractères en minuscules dans un programme C#. Cela peut être utile pour effectuer des comparaisons de chaînes de manière insensible à la casse ou pour s'assurer que toutes les données entrées par l'utilisateur sont traitées de la même manière.

## Comment faire

La conversion d'une chaîne en minuscules peut être réalisée en utilisant la méthode ToLower() de la classe String.

```C#
string mot = "Bonjour";
string motEnMinuscules = mot.ToLower();

Console.WriteLine(motEnMinuscules); // affiche "bonjour"
```

Cette méthode renvoie une nouvelle chaîne en minuscules en préservant la chaîne d'origine. Il est important de noter que cette méthode utilise la culture actuelle du système par défaut pour la conversion, donc si vous voulez une conversion spécifique, vous pouvez passer une culture en paramètre.

```C#
// conversion en minuscules en utilisant la culture française
string mot = "Bonjour";
string motEnMinuscules = mot.ToLower(new CultureInfo("fr-FR"));

Console.WriteLine(motEnMinuscules); // affiche "bonjour"
```

La méthode ToLowerInvariant() peut également être utilisée pour une conversion insensible à la culture :

```C#
string mot = "Bonjour";
string motEnMinuscules = mot.ToLowerInvariant();

Console.WriteLine(motEnMinuscules); // affiche "bonjour"
```

## Plongée en profondeur

La méthode ToLower() utilise la propriété TextInfo de la culture actuelle pour la conversion en minuscules. Cette propriété peut être modifiée pour obtenir des résultats différents.

Par exemple, si vous souhaitez que le "i" avec un point en haut (ï) soit converti en "i" sans point (i), vous pouvez modifier la casse des lettres avec la culture spécifique :

```C#
// conversion avec la casse des lettres de la culture turque
CultureInfo culture = new CultureInfo("tr-TR");
string mot = "dışarıda";
string motEnMinuscules = mot.ToLower(culture);

Console.WriteLine(motEnMinuscules); // affiche "dıŞarıda"
```

## Voir aussi

- [Documentation officielle Microsoft pour la méthode ToLower()](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.tolower)
- [Article de documentation de MSDN sur la conversion de casse en C#](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/coding-conventions-for-strings#string-case)