---
title:                "La mise en majuscule d'une chaîne"
html_title:           "C#: La mise en majuscule d'une chaîne"
simple_title:         "La mise en majuscule d'une chaîne"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Capitaliser une chaîne de caractères, c'est simplement mettre en majuscule la première lettre de chaque mot dans cette chaîne. Les programmeurs le font souvent pour améliorer la lisibilité et l'esthétique d'un code.

## Comment faire :

Voici deux façons de capitaliser une chaîne en C# :

```C#
// Méthode 1 : Utiliser la fonction ToTitleCase de la classe TextInfo
using System.Globalization;

string myString = "salut tout le monde!";
TextInfo myTextInfo = new CultureInfo("fr-FR", false).TextInfo;
string capitalizedString = myTextInfo.ToTitleCase(myString);

// Méthode 2 : Utiliser la fonction Substring et ToUpper
string myString = "salut tout le monde!";
string capitalizedString = myString.Substring(0, 1).ToUpper() + myString.Substring(1);
```

Le résultat final sera "Salut Tout Le Monde!" pour les deux méthodes.

## Zoom sur :

Dans les versions précédentes de C#, les programmeurs utilisaient souvent la méthode Char.ToUpper pour capitaliser une chaîne. Cependant, cette méthode ne fonctionnait pas correctement avec les caractères accentués. C'est pourquoi la fonction ToTitleCase a été introduite dans le .NET framework 2.0 pour résoudre ce problème.

Il existe également des alternatives pour capitaliser une chaîne en C#, telles que le Framework de Rendering de Texte WPF qui offre des fonctionnalités avancé