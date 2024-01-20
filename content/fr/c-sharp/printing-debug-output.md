---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Imprimer la sortie de débogage en C#

## Quoi & Pourquoi?  

Imprimer la sortie de débogage signifie envoyer des informations utiles du code en cours d'exécution vers une console ou un endroit similaire. Les programmeurs le font pour identifier et corriger les problèmes, ou pour comprendre ce que le programme fait à un moment précis.

## Comment faire:

Voici un exemple simple:

```C#
using System.Diagnostics;

public class Program
{
    public static void Main()
    {
        Debug.WriteLine("Ceci est une sortie de débogage.");
    }
}
```

Après avoir exécuté ce code, vous verrez "Ceci est une sortie de débogage." dans votre fenêtre de sortie de Visual Studio ou dans votre console si vous déboguez en ligne de commande.

## Plongée profonde 

Historiquement, "imprimer" était utilisé car les premiers ordinateurs imprimaient vraiment sur du papier! De nos jours, nous l'utilisons pour afficher dans la console, les fichiers logs, etc.

Il existe d'autres alternatives pour déboguer le code, comme les points d'arrêt, le débogage step-by-step, les inspecteurs de variables, etc. Cependant, l'impression de débogage reste un outil utile car elle est simple et rapide à utiliser.

En interne, `Debug.WriteLine` en C# écrit dans une ou plusieurs "écoutes", qui sont des sorties telles que la fenêtre de sortie de Visual Studio, une trace de console, etc.

## Voir aussi:

1. [Documentation Microsoft sur System.Diagnostics.Debug](https://docs.microsoft.com/fr-fr/dotnet/api/system.diagnostics.debug?view=net-5.0)

2. [Article sur les principes fondamentaux de la sortie de débogage](https://docs.microsoft.com/fr-fr/visualstudio/debugger/essential-debugging-guide?view=vs-2019)

3. [Utilisation de la sortie de débogage dans Visual Studio](https://docs.microsoft.com/fr-fr/visualstudio/debugger/how-to-use-the-debugger-feature-of-the-output-window?view=vs-2019)