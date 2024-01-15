---
title:                "Extraction de sous-chaînes"
html_title:           "C#: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaines de caractères en C#, vous avez peut-être déjà rencontré la nécessité d'extraire un certain nombre de caractères spécifiques d'une chaine plus longue. Cette opération, appelée "substrings" en anglais, est souvent utilisée pour récupérer des informations précises dans une chaine de texte. Dans cet article, nous allons explorer les différentes manières d'extraire des substrings en utilisant le langage de programmation C#.

## Comment faire

### Exemple 1 : Utiliser la méthode Substring()

La méthode Substring() de la classe String permet d'extraire une partie d'une chaine de caractères en spécifiant l'index du premier caractère et le nombre de caractères à extraire. Prenons un exemple simple :

```C#
// Déclarer une chaine de caractères
string exemple = "Ceci est un exemple";
// Extraire les 4 premiers caractères
string resultat = exemple.Substring(0, 4);
// Afficher le résultat
Console.WriteLine(resultat);
// Output : "Ceci"
```

Ici, nous avons utilisé la méthode Substring() en lui passant l'index 0 (premier caractère) et la longueur 4 (nombre de caractères à extraire). Le résultat est une nouvelle chaine de caractères contenant les 4 premiers caractères de notre exemple.

### Exemple 2 : Utiliser la méthode Split()

La méthode Split() permet de séparer une chaine de caractères en sous-chaines en utilisant un séparateur spécifique. Dans cet exemple, nous allons extraire les mots d'une phrase en utilisant l'espace comme séparateur :

```C#
// Déclarer une chaine de caractères
string phrase = "Ceci est une phrase";
// Utiliser la méthode Split()
string[] mots = phrase.Split(' ');
foreach (string mot in mots)
{
    Console.WriteLine(mot);
}
// Output : "Ceci", "est", "une", "phrase"
```

Ici, nous avons utilisé l'espace comme séparateur pour créer un tableau de sous-chaines contenant chaque mot de notre phrase. Nous pouvons ensuite parcourir ce tableau à l'aide d'une boucle foreach pour afficher chaque mot.

## Plongée en profondeur

Les méthodes Substring() et Split() sont les plus couramment utilisées pour extraire des substrings en C#, mais il existe d'autres techniques disponibles. Par exemple, vous pouvez utiliser l'indexeur de la classe String pour accéder à un caractère spécifique d'une chaine de caractères :

```C#
// Accéder au deuxième caractère d'une chaine de caractères
string exemple = "Ceci est un exemple";
char deuxiemeCaractere = exemple[1];
// Output : "e"
```

De plus, la classe Regex (pour expressions rationnelles) offre une grande flexibilité pour extraire des substrings en utilisant des motifs spécifiques.

## Voir aussi

- La documentation officielle sur les méthodes Substring() et Split() : https://docs.microsoft.com/fr-fr/dotnet/api/system.string.substring?view=netframework-4.8
https://docs.microsoft.com/fr-fr/dotnet/api/system.string.split?view=netframework-4.8
- Un tutoriel sur les expressions rationnelles en C# : https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/regular-expression-language-quick-reference