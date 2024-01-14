---
title:    "C#: Extraction de sous-chaînes"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un programmeur en herbe ou expérimenté, vous avez probablement entendu parler de l'extraction de sous-chaînes (substrings). Mais qu'est-ce que c'est et pourquoi voudriez-vous l'utiliser dans votre code ? Dans cet article, nous allons plonger dans le monde des sous-chaînes et explorer pourquoi elles sont si utiles.

# Comment faire

Pour extraire une sous-chaîne en C#, nous pouvons utiliser la méthode Substring(). Voyons un exemple concret.

```C#
string myString = "Bonjour tout le monde !";
string substring = myString.Substring(8);

Console.WriteLine(substring);
```
Le code ci-dessus extraira une sous-chaîne de la chaîne d'origine, en commençant au 8ème caractère (notez que le premier caractère est compté comme 0). Dans cet exemple, la sortie sera "tout le monde !".

Mais que se passe-t-il si nous voulons extraire une sous-chaîne à partir d'une position spécifique et d'une longueur donnée ? Nous pouvons le faire en utilisant une surcharge de la méthode Substring() qui accepte deux paramètres : l'index de départ et la longueur de la sous-chaîne.

```C#
string myString = "Bonjour tout le monde !";
string substring = myString.Substring(0, 7);

Console.WriteLine(substring);
```
La sortie de ce code sera "Bonjour", car nous avons spécifié que la sous-chaîne doit commencer au premier caractère et avoir une longueur de 7 caractères.

# Plongée en profondeur

L'une des raisons pour lesquelles les sous-chaînes sont si utiles est qu'elles permettent de manipuler facilement une partie d'une chaîne de caractères sans avoir à réécrire la chaîne complète. En utilisant la méthode Substring(), nous pouvons extraire des morceaux de texte, les modifier et les réinsérer dans la chaîne d'origine.

De plus, les sous-chaînes sont largement utilisées pour traiter les données entrées par l'utilisateur, en extrayant des informations spécifiques telles que des noms, des dates ou des numéros de téléphone.

# Voir aussi

- [Documentation Microsoft pour la méthode Substring() en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.string.substring?view=netcore-3.1)
- [Exemple d'utilisation de la méthode Substring()](https://www.tutorialspoint.com/csharp/csharp_string_substring.htm)
- [Utiliser les sous-chaînes pour manipuler du texte en C#](https://www.c-sharpcorner.com/article/11-ways-to-manipulate-the-c-sharp-string/)

Merci d'avoir lu cet article sur l'utilisation des sous-chaînes en C#! Nous espérons que vous pourrez maintenant utiliser cette fonctionnalité utile dans vos propres projets de programmation. N'oubliez pas de consulter les liens ci-dessus pour en savoir plus sur les sous-chaînes et comment les utiliser dans votre code. À la prochaine !