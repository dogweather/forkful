---
title:                "C#: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières peuvent sembler effrayantes pour les débutants en programmation, mais elles sont incroyablement utiles pour créer des patrons dans du texte et les manipuler selon nos besoins. Elles sont utilisées pour la validation des entrées utilisateur, la recherche et la manipulation de données, et bien plus encore.

## Comment faire

Voici un exemple simple montrant comment utiliser les expressions régulières en C# pour valider un numéro de téléphone :

```C#
string regex = @"^\(?([0-9]{3})\)?[- .]?([0-9]{3})[- .]?([0-9]{4})$";

string phoneNumber = "123-456-7890";
if (Regex.IsMatch(phoneNumber, regex))
{
  Console.WriteLine("Le numéro de téléphone est valide !");
}
else
{
  Console.WriteLine("Le numéro de téléphone est invalide.");
}

Output: Le numéro de téléphone est valide !
```

Dans cet exemple, nous créons une expression régulière qui correspond à un numéro de téléphone américain au format xxx-xxx-xxxx. Nous utilisons ensuite la méthode `IsMatch` de la classe `Regex` pour vérifier si le numéro de téléphone fourni correspond à notre expression régulière.

Il existe de nombreux autres opérateurs et astuces pour créer des expressions régulières plus complexes en C#, mais avec cet exemple simple, vous êtes maintenant en mesure de les utiliser dans vos propres projets.

## Plongée en profondeur

Pour de plus amples informations et une liste complète des opérateurs et des symboles que vous pouvez utiliser dans vos expressions régulières C#, vous pouvez consulter la documentation officielle de Microsoft : [Expressions régulières en .NET](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expressions).

Un autre outil utile pour tester et vérifier vos expressions régulières est [Regex101](https://regex101.com/), qui vous permet également de créer des expressions régulières pour d'autres langages de programmation tels que JavaScript et Python.

## Voir également

- [Expressions régulières en .NET (Microsoft)](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expressions)
- [Regex101](https://regex101.com/)