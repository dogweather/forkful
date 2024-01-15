---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "C#: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou expérimenté, il est important de comprendre les bases du langage C#. L'une de ces bases est de savoir comment trouver la longueur d'une chaîne de caractères. Que vous construisiez une application ou manipuliez des données, la connaissance de cette compétence peut vous faire gagner du temps et améliorer la qualité de votre code.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en C#, vous pouvez utiliser la méthode intégrée "Length". Elle accepte en entrée une chaîne de caractères et renvoie un entier représentant le nombre de caractères dans la chaîne. Voici un exemple de code :

```C#
string phrase = "Bonjour les programmeurs!";
int longueur = phrase.Length;
Console.WriteLine("La longueur de la chaîne est de " + longueur + " caractères.");
```

Cet exemple déclarera une variable "phrase" contenant la chaîne de caractères "Bonjour les programmeurs!" et une variable "longueur" qui stockera le résultat de la méthode "Length". Ensuite, la ligne "Console.WriteLine" affichera la longueur de la chaîne dans la console. 

## Plongée en profondeur

Saviez-vous que la méthode "Length" utilise en fait un compteur interne de la classe String ? Ce compteur est mis à jour chaque fois qu'une modification est apportée à la chaîne de caractères, ce qui permet de récupérer la longueur de manière plus efficace. De plus, cette méthode fonctionne avec tous les types de chaînes de caractères, qu'il s'agisse d'une chaîne brute, d'une chaîne de caractères interpolée ou même d'une chaîne stockée dans une variable. En comprenant le fonctionnement interne de cette méthode, vous serez en mesure de l'utiliser de manière plus efficace dans vos programmes. 

## Voir aussi

- Documentation officielle de la méthode "Length" en C# : https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0
- Tutoriel sur les chaînes de caractères en C# : https://www.tutorialspoint.com/csharp/csharp_strings.htm
- Exemples pratiques pour utiliser la méthode "Length" : https://www.c-sharpcorner.com/article/all-about-string-in-c-sharp/