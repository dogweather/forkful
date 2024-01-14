---
title:    "C#: La mise en majuscule d'une chaîne"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La capitalisation d'une chaîne de caractères peut sembler être une tâche simple et insignifiante. Cependant, cela peut être très utile dans certaines situations, comme lors de la manipulation de données ou lors de l'affichage de texte à l'utilisateur. Dans cet article, nous allons expliquer pourquoi il est important de savoir comment capitaliser une chaîne en utilisant le langage de programmation C#.

## Comment Faire

À l'aide de quelques exemples codés en C#, nous allons expliquer comment capitaliser une chaîne de caractères. Tout d'abord, il est important de savoir que la méthode .ToUpper() permet de mettre tous les caractères d'une chaîne en majuscules. Nous pouvons l'utiliser de la manière suivante :

```C#
string message = "bonjour";
Console.WriteLine(message.ToUpper());
```

Cela produira une sortie de "BONJOUR". De même, la méthode .ToLower() mettra tous les caractères en minuscules. Jetons maintenant un coup d'œil à la méthode .ToTitleCase(), qui met en majuscule la première lettre de chaque mot dans la chaîne.

```C#
string phrase = "je suis un programmeur";
TextInfo myTI = new CultureInfo("fr-FR", false).TextInfo;
Console.WriteLine(myTI.ToTitleCase(phrase));
```

Cela produira une sortie de "Je Suis Un Programmeur". Notez que nous avons utilisé une nouvelle instance de la classe TextInfo, qui nous permet d'utiliser les règles de capitalisation spécifiques à la culture française.

## Plongée en Profondeur

Maintenant que nous avons vu comment utiliser ces méthodes pour capitaliser une chaîne, il est important de comprendre comment elles fonctionnent en profondeur. La méthode .ToUpper() utilise la table de code ASCII pour convertir les caractères en majuscules en utilisant une simple addition de 32 à la valeur numérique de chaque caractère en minuscule. La méthode .ToLower() utilise une soustraction de 32 pour réaliser l'opération inverse. Pour la méthode .ToTitleCase(), elle utilise les règles de capitalisation spécifiques à chaque culture, qui peuvent être vues en utilisant l'utilitaire de ligne de commande "Character Map" sous Windows.

## Voir Aussi

- [Documentation officielle C# pour les méthodes de capitalisation de chaînes](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.totitlecase?view=netcore-3.1)
- [Table de code ASCII](https://fr.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)
- [Utilitaire de ligne de commande "Character Map" sous Windows](https://support.microsoft.com/fr-fr/help/17424/windows-change-keyboard-layout)