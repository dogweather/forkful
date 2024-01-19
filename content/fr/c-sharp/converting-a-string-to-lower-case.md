---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Article: Convertir une chaîne en minuscules en C#

## Qu'est-ce que c'est et Pourquoi?

La conversion d'une chaîne en minuscules est un processus qui transforme tous les caractères majuscules en minuscules dans une chaîne de caractères. Les programmeurs l'utilisent pour normaliser le texte avant de le comparer, de le rechercher, ou de l'utiliser dans d'autres opérations sensibles à la casse.

## Comment faire:

Pour convertir une chaîne en minuscules en C#, on utilise la méthode ToLower(). Voici comment ça marche:

```C#
string maChaine = "BONJOUR, LE MONDE!";
string maChaineEnMinuscules = maChaine.ToLower();
Console.WriteLine(maChaineEnMinuscules);
```

Dans cet exemple, la sortie sera "bonjour, le monde!".

## Plongée en profondeur :

Historiquement, la conversion en minuscules a été utilisée pour faciliter la lecture et le traitement des textes par les humains et les ordinateurs. Avec l'évolution des langages de programmation, cette opération est devenue plus simple et plus performante.

Il existe des alternatives à la méthode ToLower() en C#, telles que la méthode ToLowerInvariant(). Elle convertit également une chaîne en minuscules, mais elle ne prend pas en compte les spécificités régionales de l'écriture. Par exemple:

```C#
string maChaine = "BONJOUR, LE MONDE!";
string maChaineEnMinuscules = maChaine.ToLowerInvariant();
Console.WriteLine(maChaineEnMinuscules);
```

La sortie de cet exemple serait aussi "bonjour, le monde!".

Les détails d'implémentation de ToLower() sont basés sur la bibliothèque standard .NET de Microsoft, qui est utilisée par le langage C#. Elle offre une prise en charge complète de la manipulation de chaînes de caractères, y compris la conversion en minuscules.


## Voir aussi :

Pour plus d'informations sur la manipulation des chaînes en C#, consultez ces ressources :

- La documentation officielle de Microsoft sur les chaînes en C# : https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/strings/
- StackOverflow, pour des exemples de code et des discussions sur la manipulation des chaînes: https://stackoverflow.com/questions/tagged/c%23+string
- Le blog de Jon Skeet sur les chaînes en C#: http://csharpindepth.com/Articles/StringsAndText.aspx