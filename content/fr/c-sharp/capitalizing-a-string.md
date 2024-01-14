---
title:    "C#: Capitaliser une chaîne de caractères"
keywords: ["C#"]
---

{{< edit_this_page >}}

Pourquoi: Il arrive souvent que l'on veuille mettre en majuscule une chaîne de caractères en programmation. Cela peut être nécessaire pour des raisons de lisibilité ou pour répondre à une exigence spécifique du projet. Dans cet article, nous allons apprendre comment capitaliser facilement une chaîne de caractères en C#.

Comment faire: Il existe plusieurs façons de capitaliser une chaîne de caractères en C#. Nous allons ici en explorer trois: en utilisant la méthode ToUpper(), en utilisant la méthode Substring() et en utilisant une boucle for pour parcourir la chaîne caractère par caractère.

```C#
// Utiliser la méthode ToUpper()
string message = "bonjour tout le monde";
string capitalizedMessage = message.ToUpper();

Console.WriteLine(capitalizedMessage); // Affiche "BONJOUR TOUT LE MONDE"

// Utiliser la méthode Substring()
string message = "bonjour tout le monde";
string firstLetter = message.Substring(0, 1).ToUpper();
string capitalizedMessage = firstLetter + message.Substring(1);

Console.WriteLine(capitalizedMessage); // Affiche "Bonjour tout le monde"

// Utiliser une boucle for
string message = "bonjour tout le monde";
string capitalizedMessage = "";

for (int i = 0; i < message.Length; i++)
{
    if (i == 0)
    {
        capitalizedMessage += Char.ToUpper(message[i]);
    }
    else
    {
        capitalizedMessage += message[i];
    }
}

Console.WriteLine(capitalizedMessage); // Affiche "Bonjour tout le monde"
```

Comme on peut le voir dans ces exemples, il est possible de capitaliser une chaîne de caractères de différentes manières en C#. Il suffit de trouver celle qui convient le mieux à votre code et à vos besoins.

Plongée en profondeur: Bien que capitaliser une chaîne de caractères puisse sembler simple, il y a en réalité beaucoup de choses à prendre en compte. Par exemple, si la chaîne contient des caractères spéciaux ou accentués, il peut y avoir des problèmes lors de la capitalisation. Il est donc important d'utiliser des méthodes spécifiques, comme ToLower() et ToUpperInvariant(), qui prennent en compte ces cas particuliers.

En outre, il peut également être utile de créer une méthode spécifique pour capitaliser une chaîne de caractères et l'utiliser à plusieurs reprises dans votre code, plutôt que de répéter les mêmes lignes de code à chaque fois.

Voir aussi: Si vous souhaitez en savoir plus sur la manipulation de chaînes de caractères en C#, voici quelques liens utiles:

- [Documentation officielle sur les chaînes de caractères en C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [Tutoriel sur les opérations de base sur les chaînes en C#](https://www.tutorialspoint.com/csharp/csharp_strings.htm)
- [Vidéo explicative sur les méthodes de capitalisation en C#](https://www.youtube.com/watch?v=xeUIBjdDkcA)