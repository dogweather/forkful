---
title:                "C#: Mettre en majuscule une chaîne de caractères"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut sembler anodin, mais il est important de savoir comment capitaliser correctement une chaîne de caractères en C#. Cela peut sembler être une tâche simple, mais une mauvaise capitalisation peut entraîner des erreurs dans votre code et des résultats inattendus. Heureusement, il existe des méthodes simples pour capitaliser efficacement une chaîne de caractères en C#, que nous allons explorer dans cet article.

## Comment Faire

Pour capitaliser une chaîne de caractères en C#, vous pouvez utiliser la méthode `ToUpper()` ou `ToLower()` en fonction de vos besoins.

```
string phrase = "Bonjour le monde";

Console.WriteLine(phrase.ToUpper());
Console.WriteLine(phrase.ToLower());

// Output:
// BONJOUR LE MONDE
// bonjour le monde
```

Vous pouvez également utiliser la méthode `CultureInfo` pour capitaliser une chaîne de caractères selon une culture spécifique.

```
string phrase = "hello world";

Console.WriteLine(CultureInfo.CurrentCulture.TextInfo.ToTitleCase(phrase));

// Output:
// Hello World
```

## Plongée en Profondeur

Il est important de noter que la méthode `ToUpper()` et `ToLower()` utilisent la culture en cours pour capitaliser la chaîne de caractères. Si vous voulez une capitalisation spécifique, vous pouvez utiliser la méthode `String.ToUpperInvariant()` ou `String.ToLowerInvariant()`.

```
string phrase = "Bonjour le monde";

Console.WriteLine(phrase.ToUpper());
Console.WriteLine(phrase.ToUpperInvariant());

// Output:
// BONJOUR LE MONDE
// BONJOUR LE MONDE
```

De plus, il est important de comprendre l'utilisation de la méthode `CultureInfo` pour capitaliser une chaîne de caractères. Par exemple, le comportement de la méthode `ToTitleCase()` peut varier en fonction de la culture utilisée. Prenons pour exemple le mot "Istanbul" qui sera capitalisé différemment selon la culture choisie.

```
string city = "Istanbul";

CultureInfo english = new CultureInfo("en-US");
CultureInfo turkish = new CultureInfo("tr-TR");

Console.WriteLine(english.TextInfo.ToTitleCase(city));
Console.WriteLine(turkish.TextInfo.ToTitleCase(city));

// Output:
// Istanbul
// İstanbul
```

Comme vous pouvez le voir, la méthode `ToTitleCase()` capitalise le "I" en minuscule en utilisant la culture turque, tandis qu'il reste en majuscule en utilisant la culture anglaise.

## Voir aussi

- [Documentation Microsoft sur la méthode ToUpper()](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.toupper)
- [Documentation Microsoft sur la méthode ToLower()](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.tolower)
- [Documentation Microsoft sur la classe CultureInfo](https://docs.microsoft.com/fr-fr/dotnet/api/system.globalization.cultureinfo)