---
title:    "C#: Écrire vers l'erreur standard"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en C#, il est très probable que vous ayez rencontré la syntaxe `Console.Error.WriteLine()` lors de l'écriture de votre code. Mais pourquoi écrire à la sortie d'erreur standard en premier lieu? Dans cet article, nous allons explorer l'utilisation de la sortie d'erreur standard et pourquoi elle peut être utile dans votre programmation en C#.

## Comment Faire

Commençons par un exemple simple pour voir comment utiliser `Console.Error.WriteLine()` dans un code C#. Supposons que nous avons le code suivant:

```
using System;

class Program
{
    static void Main()
    {
        int num = 10;
        if (num < 0)
        {
            Console.Error.WriteLine("Le nombre est inférieur à 0.");
        }
    }
}
```

Si nous exécutons ce code et que `num` est effectivement inférieur à 0, nous verrons le message "Le nombre est inférieur à 0." imprimé sur la sortie d'erreur standard plutôt que la sortie standard normale.

Mais pourquoi est-ce utile? Eh bien, cela peut être particulièrement utile en débogage. En écrivant à la sortie d'erreur standard, nous sommes en mesure de distinguer facilement les messages d'erreur et les messages normaux, ce qui peut être très utile pour trouver et corriger les erreurs dans notre code.

En outre, il peut être utile de rediriger la sortie d'erreur standard vers un fichier plutôt que de laisser les messages s'afficher dans la console. Cela peut être fait en utilisant la commande `2>` en ligne de commande ou en utilisant la classe `StreamWriter` en C#.

## Plongée Profonde

En plus de simplement imprimer des messages d'erreur, la sortie d'erreur standard peut également être utilisée pour renvoyer des codes d'erreur spécifiques à votre programme. Par exemple, vous pouvez utiliser `Console.Error.WriteLine()` pour imprimer un message d'erreur et ensuite utiliser `Environment.Exit()` pour renvoyer un code d'erreur spécifique au système d'exploitation.

De plus, vous pouvez également personnaliser complètement l'apparence de votre message d'erreur en utilisant des couleurs et en ajoutant votre propre formatage. Cela peut rendre vos messages d'erreur plus visuellement attrayants et faciles à comprendre pour les utilisateurs.

## Voir Aussi

- [Documentation officielle de Microsoft sur la sortie d'erreur standard en C#](https://docs.microsoft.com/fr-fr/dotnet/api/system.console.error)
- [Article StackOverflow sur la redirection des messages d'erreur en C#](https://stackoverflow.com/questions/724712/redirection-of-console-in-net)
- [Tutoriel YouTube sur l'utilisation de couleurs pour la sortie d'erreur standard en C#](https://www.youtube.com/watch?v=_jXxXUQ4bL8)