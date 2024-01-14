---
title:                "C#: Recherche et remplacement de texte"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

L'une des tâches les plus courantes lors de la programmation est de trouver et remplacer du texte dans du code. Que ce soit pour corriger des erreurs, modifier des variables ou mettre à jour du contenu, la recherche et le remplacement de texte sont indispensables pour maintenir un code propre et fonctionnel.

# Comment faire

La recherche et le remplacement de texte peuvent sembler simples, mais il y a plusieurs façons de les effectuer en utilisant le langage de programmation C#. Voici quelques exemples de code pour vous montrer comment cela peut être réalisé.

```C#
// Remplacement d'une chaîne de caractères dans une autre
string texte = "Bonjour le monde !";
string nouveauTexte = texte.Replace("Bonjour", "Salut");
Console.WriteLine(nouveauTexte); // Affichera "Salut le monde !"

// Recherche et remplacement en utilisant les expressions régulières
Regex regex = new Regex(@"\d+");
string numero = "Il y a 3 chats dans la maison";
string resultat = regex.Replace(numero, "2");
Console.WriteLine(resultat); // Affichera "Il y a 2 chats dans la maison"
```

# Plongée en profondeur

Bien qu'il existe de nombreuses façons d'effectuer une recherche et un remplacement de texte en utilisant C#, il est important de comprendre comment cela fonctionne en interne. Lorsque vous effectuez une recherche et un remplacement, vous créez essentiellement une nouvelle chaîne de caractères en copiant la chaîne originale et en remplaçant les parties souhaitées.

Il est également important de prendre en compte les performances lorsque vous utilisez la recherche et le remplacement de texte dans votre code. Les expressions régulières peuvent être très utiles, mais elles peuvent également être coûteuses en termes de temps de traitement.

# Voir aussi

Pour en savoir plus sur la recherche et le remplacement de texte en utilisant C#, vous pouvez consulter les liens suivants :

- La documentation officielle de Microsoft sur le remplacement de texte en utilisant Regex : https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/substitution-in-regular-expressions
- Tutoriel vidéo sur la recherche et le remplacement en utilisant C# : https://www.youtube.com/watch?v=q9nz_M7c9g0
- Un exemple de projet GitHub utilisant la recherche et le remplacement de texte en C# : https://github.com/FilipDutescu/SimpleTextEditor