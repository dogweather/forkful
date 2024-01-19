---
title:                "Mettre en majuscule une chaîne"
html_title:           "C#: Mettre en majuscule une chaîne"
simple_title:         "Mettre en majuscule une chaîne"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

### Qu'est-ce & Pourquoi ?

Capitaliser une chaîne de caractères signifie transformer la première lettre de chaque mot en majuscule. Les programmeurs le font souvent pour normaliser des données d'entrée ou améliorer la lisibilité.

## Comment faire :

Voici un exemple de code sur la façon de capitaliser une chaîne en C#:

```C#
public static void Main()
{
    string maPhrase = "je suis un développeur.";
    string resultat = System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(maPhrase);
    
    Console.WriteLine(resultat); // "Je Suis Un Développeur."
}
```
Dans cet exemple, nous utilisons la méthode `ToTitleCase` du `CultureInfo.CurrentCulture.TextInfo` pour capitaliser chaque mot dans la chaîne `maPhrase`.

## Deep Dive 

L'origine de la capitalisation remonte à l'époque du papier et du stylo, mais elle reste un outil commun dans la programmation pour améliorer la lisibilité et normaliser les entrées.

En C#, il existe plusieurs méthodes pour capitaliser une chaîne. Une autre méthode couramment utilisée pour capitaliser les mots d'une chaîne utilise `String.Split` et `TextInfo.ToTitleCase`. Toutefois, cette dernière méthode ne change que les premières lettres des mots de la chaîne en majuscule et laisse les autres lettres en minuscule.

L'implémentation de `ToTitleCase` en C# modifie uniquement les lettres minuscules qui précèdent un caractère non alphabétique (par exemple, un espace) en lettres majuscules. C’est pourquoi elle est plus efficace pour convertir des phrases ou des titres, mais pas idéale pour des utilisations plus générales.

## Voir Aussi 

Pour plus de détails sur la gestion des chaînes en C#, consultez ces ressources :
1. Documentation Microsoft : [String manipulation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/modify-string-contents)
2. Stack Overflow: [How to capitalize the first character of each word in a string (C#)](https://stackoverflow.com/questions/732531/is-there-a-name-for-the-c-textinfo-totitlecase-function-capability)