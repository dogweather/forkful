---
title:                "Utilisation des tableaux associatifs"
aliases:
- /fr/c-sharp/using-associative-arrays/
date:                  2024-01-30T19:10:13.451681-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des tableaux associatifs"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Les tableaux associatifs, ou dictionnaires en C#, vous permettent de stocker et de gérer des paires de clés et de valeurs. Ils sont votre solution privilégiée lorsque vous avez besoin de récupérer rapidement des valeurs basées sur un identifiant unique, rendant la gestion des données un jeu d'enfant dans les applications complexes.

## Comment faire :

En C#, vous travaillez avec les tableaux associatifs en utilisant la classe `Dictionary<TKey, TValue>`. Voici un exemple rapide pour vous lancer :

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Création d'un dictionnaire
        Dictionary<string, int> panierDeFruits = new Dictionary<string, int>();

        // Ajout de paires clé-valeur
        panierDeFruits.Add("Pommes", 5);
        panierDeFruits.Add("Oranges", 10);

        // Accès à une valeur en utilisant sa clé
        Console.WriteLine("Pommes : " + panierDeFruits["Pommes"]);
        
        // Mise à jour d'une valeur
        panierDeFruits["Pommes"] = 7;
        Console.WriteLine("Pommes mises à jour : " + panierDeFruits["Pommes"]);
        
        // Suppression d'une paire clé-valeur
        panierDeFruits.Remove("Oranges");

        // Itération sur le dictionnaire
        foreach (var paire in panierDeFruits)
        {
            Console.WriteLine(paire.Key + " : " + paire.Value);
        }
    }
}
```
Sortie d'exemple :
```
Pommes : 5
Pommes mises à jour : 7
Pommes : 7
```

Cet exemple illustre la création d'un dictionnaire, l'ajout, l'accès, la mise à jour, et la suppression d'éléments, ainsi que l'itération sur celui-ci.

## Plongée profonde

Le concept de tableaux associatifs remonte à leur utilisation dans des langages de script comme Perl et PHP, où ils offrent une flexibilité dans la gestion des collections de données. En C#, `Dictionary<TKey, TValue>` est l'implémentation de facto, introduite dans .NET Framework 2.0. Il stocke les données dans une table de hachage, assurant des recherches, ajouts, et suppressions efficaces.

Cependant, il convient de noter que, bien que les dictionnaires soient incroyablement polyvalents, ils ne seront pas toujours votre meilleure option. Pour maintenir des collections ordonnées, vous pourriez envisager `SortedDictionary<TKey, TValue>` ou `SortedList<TKey, TValue>`, qui offrent un ordre trié au prix d'opérations d'insertion et de suppression plus lentes. Pour les scénarios nécessitant la sécurité des threads, `ConcurrentDictionary<TKey, TValue>` ajoute de la surcharge mais assure un accès sécurisé depuis plusieurs threads sans verrouillage manuel.

En fin de compte, le choix d'une implémentation de tableau associatif en C# dépend de vos besoins spécifiques concernant l'ordre, la performance, et la sécurité des threads.
