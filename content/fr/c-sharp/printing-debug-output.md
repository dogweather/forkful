---
title:                "Impression de sortie de débogage"
html_title:           "C#: Impression de sortie de débogage"
simple_title:         "Impression de sortie de débogage"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

La mise en place de débogage dans un code peut être un outil extrêmement utile pour les programmeurs. Cela permet de suivre l'exécution du code, d'identifier les erreurs et de résoudre rapidement les bugs. En bref, c'est un moyen efficace pour améliorer la qualité du code et faciliter le processus de débogage.

## Comment faire

Pour imprimer une sortie de débogage dans un code C#, il suffit d'utiliser la méthode `System.Diagnostics.Debug.WriteLine()`. Cette méthode prend en paramètre une expression ou une variable et l'affiche dans la fenêtre de débogage. Voyons un exemple simple :

```C#
int num1 = 5;
int num2 = 10;

System.Diagnostics.Debug.WriteLine($"Le résultat de l'addition est : {num1 + num2}");
```

Lorsque vous exécutez ce code, vous verrez la ligne suivante s'afficher dans la fenêtre de débogage :

```
Le résultat de l'addition est : 15
```

Vous pouvez également utiliser la méthode `System.Diagnostics.Debug.Write()` pour imprimer une sortie sans saut de ligne. Cela peut être utile pour afficher des valeurs dans une boucle, par exemple :

```C#
for (int i = 0; i < 10; i++)
{
    System.Diagnostics.Debug.Write(i + " ");
}
```

Ce code affichera les nombres de 0 à 9 sur une seule ligne dans la fenêtre de débogage.

Si vous souhaitez désactiver temporairement l'impression de la sortie de débogage, vous pouvez utiliser la directive de préprocesseur suivante :

```C#
#define DEBUG

// Votre code ici

#if DEBUG
System.Diagnostics.Debug.WriteLine("Cette ligne ne sera imprimée que si DEBUG est défini");
#endif
```

## Plongée en profondeur

La méthode `System.Diagnostics.Debug.WriteLine()` est très utile, mais elle peut être encore plus efficace si vous combinez son utilisation avec des informations de débogage supplémentaires. Par exemple, vous pouvez inclure le nom de la méthode et le numéro de ligne où le message est imprimé en utilisant la classe `System.Diagnostics.StackTrace`. Cela peut être particulièrement utile lors du débogage de code multi-thread, car vous pouvez savoir exactement quelle ligne de code a déclenché la sortie.

Voici un exemple de code utilisant `System.Diagnostics.StackTrace` :

```C#
System.Diagnostics.StackTrace stackTrace = new System.Diagnostics.StackTrace(true);

System.Diagnostics.Debug.WriteLine($"Erreur dans la méthode {stackTrace.GetFrame(0).GetMethod().Name} à la ligne {stackTrace.GetFrame(0).GetFileLineNumber()}");
```

Ce code affichera une sortie comme celle-ci :

```
Erreur dans la méthode Main à la ligne 10
```

Vous pouvez également utiliser la classe `System.Diagnostics.Debug` pour créer différents niveaux de débogage, en utilisant les méthodes `WriteIf()` et `Assert()`. Ces méthodes prennent toutes deux un paramètre de condition ou une expression booléenne et n'impriment que si cette condition est vraie. Cela peut vous aider à cibler des parties spécifiques de votre code pour un débogage plus efficace.

## Voir aussi

- [Guide de débogage en C#](https://docs.microsoft.com/fr-fr/visualstudio/debugger/debugger-feature-tour?view=vs-2019)
- [Utilisation de la méthode WriteLine dans la documentation Microsoft](https://docs.microsoft.com/fr-fr/dotnet/api/system.diagnostics.debug.writeline?view=net-5.0)