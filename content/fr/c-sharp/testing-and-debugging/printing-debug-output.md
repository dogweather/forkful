---
date: 2024-01-20 17:52:29.187512-07:00
description: "How to: (Comment faire :) Historiquement, le d\xE9bogage se faisait\
  \ avec des LED ou des impressions sur papier. Aujourd'hui, `Console.WriteLine` est\
  \ la\u2026"
lastmod: '2024-04-05T22:51:11.784251-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Historiquement, le d\xE9bogage se faisait avec des LED\
  \ ou des impressions sur papier."
title: "Affichage des sorties de d\xE9bogage"
weight: 33
---

## How to: (Comment faire :)
```C#
using System;

class Program
{
    static void Main()
    {
        // Afficher un message simple
        Console.WriteLine("Début du débogage.");

        // Utiliser la concaténation de chaînes pour les sorties complexes
        for (int i = 0; i < 3; i++)
        {
            Console.WriteLine("Indice de boucle: " + i);
        }

        // Si besoin, incluez des valeurs de variables
        int a = 5;
        int b = 10;
        Console.WriteLine($"La somme de {a} et {b} est {a+b}.");

        Console.WriteLine("Fin du débogage.");
    }
}
```
**Sortie :**
```
Début du débogage.
Indice de boucle: 0
Indice de boucle: 1
Indice de boucle: 2
La somme de 5 et 10 est 15.
Fin du débogage.
```

## Deep Dive (Plongée en profondeur)
Historiquement, le débogage se faisait avec des LED ou des impressions sur papier. Aujourd'hui, `Console.WriteLine` est la méthode basique pour afficher les sorties dans C#. Il existe aussi `Debug` et `Trace` dans `System.Diagnostics` qui offrent plus de contrôle et la possibilité de désactiver les sorties en production.

Dans les IDE modernes, on peut aussi utiliser des points d'arrêt et inspecter les variables "en live". Mais ces sorties de débogues continuent d'être utiles pour les journaux ou le débogage sur un système sans IDE.

Pour une application moins intrusive sur les performances, `Console.WriteLine` peut ralentir l'exécution s'il est trop utilisé. C'est pourquoi les logs sont parfois préférés en production, offrant un compromis entre l'info et les performances.

## See Also (Voir aussi)
- [Documentation Microsoft sur l'écriture dans une console](https://docs.microsoft.com/fr-fr/dotnet/api/system.console.writeline)
- [Documentation sur la classe Debug](https://docs.microsoft.com/fr-fr/dotnet/api/system.diagnostics.debug)
- [Présentation des points d'arrêt dans Visual Studio](https://docs.microsoft.com/fr-fr/visualstudio/debugger/using-breakpoints)
- [Stack Overflow: Quand utiliser Console.WriteLine vs Debug.WriteLine?](https://stackoverflow.com/questions/118318/when-should-i-use-console-writeline-vs-debug-writeline)
