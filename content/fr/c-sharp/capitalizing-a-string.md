---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "C#: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes programmeur en C# ou si vous souhaitez apprendre ce langage, vous avez probablement entendu parler de la manipulation de chaînes de caractères. L'une des actions courantes lors du traitement de chaînes de caractères est la mise en majuscule de certaines parties. Dans cet article, nous allons nous concentrer sur la façon de mettre des chaînes de caractères en majuscules en utilisant le langage C#.

## Comment faire

Pour mettre une chaîne de caractères en majuscule en C#, vous pouvez utiliser la méthode *ToUpper()* de la classe *string*. Voici un exemple de code qui illustre son utilisation :

```C#
string message = "Bonjour à tous !";
string messageMajuscule = message.ToUpper();
Console.WriteLine(messageMajuscule);
```

Lorsque vous exécutez ce code, la phrase "Bonjour à tous !" sera affichée en majuscule dans la console : "BONJOUR À TOUS !"

Vous pouvez également utiliser la méthode *ToUpper()* pour mettre en majuscule une partie spécifique d'une chaîne de caractères en utilisant des index. Voici un exemple :

```C#
string message = "Bonjour à tous !";
string debut = message.Substring(0, 8); // sélectionne les 8 premiers caractères
string messageMajuscule = debut.ToUpper() + message.Substring(8); // met en majuscule les 8 premiers caractères
Console.WriteLine(messageMajuscule);
```

Le résultat de ce code sera "BONJOUR à tous !".

## Plongée en profondeur

Maintenant que vous savez comment mettre des chaînes de caractères en majuscule, il est important de comprendre que cette manipulation est utile dans de nombreuses situations. En modifiant la casse d'une chaîne, vous pouvez par exemple vérifier si elle contient une certaine sous-chaîne sans tenir compte de la casse. Cela peut également faciliter la comparaison de chaînes dans différents formats. De plus, la mise en majuscule peut être utile dans des cas où la lisibilité est importante, comme pour des messages d'erreur ou des titres.

Il existe également d'autres méthodes pour manipuler la casse des chaînes en C#, comme *ToLower()* et *ToTitleCase()*. Vous pouvez également utiliser l'attribut *ToUpper* au moment de définir une chaîne pour qu'elle soit directement en majuscule, comme ceci :

```C#
string message = "BONJOUR À TOUS !";
```

La méthode *ToUpper()* et les autres méthodes de manipulation de la casse sont toutes documentées dans la documentation officielle de Microsoft pour le langage C#.

## Voir aussi

- [Documentation officielle Microsoft pour les méthodes de manipulation de la casse en C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Comment mettre en majuscule une chaîne de caractères en C#](https://www.abrivard.com/transformer-chaîne-majuscule-méthode-c-sharp/)
- [Tutoriel vidéo pour débutants sur la manipulation de chaînes en C#](https://www.youtube.com/watch?v=qqNZxFM-9rE)