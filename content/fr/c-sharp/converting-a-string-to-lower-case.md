---
title:                "C#: Conversion d'une chaîne en minuscules"
simple_title:         "Conversion d'une chaîne en minuscules"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous travaillez avec des chaînes de caractères dans vos programmes en C#, il peut parfois être nécessaire de les convertir en minuscules. Cela peut être utile pour faciliter les comparaisons de chaînes de caractères ou pour simplement uniformiser la casse dans votre code. Dans cet article, nous allons explorer comment convertir une chaîne de caractères en minuscules en utilisant le langage de programmation C#.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en C#, vous pouvez utiliser la méthode `ToLower()` de la classe `String`. Voici un exemple de code :

```
string str = "Bonjour le monde !";
string lowerStr = str.ToLower(); // lowerStr contiendra "bonjour le monde !"
```

Comme vous pouvez le voir, en utilisant la méthode `ToLower()`, la chaîne de caractères originale est convertie en minuscules et le résultat est stocké dans une nouvelle variable.

Dans le cas où vous souhaitez convertir une chaîne de caractères en minuscules sans créer de nouvelle variable, vous pouvez également utiliser la méthode `ToLower()` directement sur la chaîne de caractères d'origine :

```
string str = "Bonjour le monde !";
str = str.ToLower();
// str contiendra maintenant "bonjour le monde !"
```

## Plongée en profondeur

Lorsque vous utilisez la méthode `ToLower()` pour convertir une chaîne de caractères en minuscules en C#, il est important de noter que cette méthode utilise les paramètres régionaux de votre ordinateur pour effectuer la conversion. Cela signifie que le résultat peut varier en fonction de la langue de votre système d'exploitation.

De plus, la méthode `ToLower()` ne convertira que les caractères anglais en minuscules. Pour convertir les caractères unicode en minuscules, vous devrez utiliser la méthode `ToLowerInvariant()`.

Enfin, il est important de noter que la méthode `ToLower()` ne modifie pas la chaîne de caractères d'origine, mais plutôt qu'elle renvoie une nouvelle chaîne de caractères avec la casse modifiée. Pour modifier directement la chaîne de caractères d'origine, vous devrez réassigner la valeur retournée par la méthode `ToLower()` comme vu dans l'exemple précédent.

## Voir aussi

- [Documentation Microsoft sur la méthode `ToLower()`](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.tolower)
- [Documentation Microsoft sur la méthode `ToLowerInvariant()`](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.tolowerinvariant)
- [ASCII Table](https://fr.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)