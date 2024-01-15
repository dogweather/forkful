---
title:                "La lecture des arguments de la ligne de commande"
html_title:           "C#: La lecture des arguments de la ligne de commande"
simple_title:         "La lecture des arguments de la ligne de commande"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur en C#, vous vous êtes sûrement déjà retrouvé dans une situation où vous avez besoin de fournir des arguments en ligne de commande à votre programme. Cet article vous expliquera pourquoi il est important de savoir lire les arguments en ligne de commande et comment le faire efficacement.

## Comment faire

```C#
static void Main(string[] args)
{
    // Votre programme commence ici
    // La variable 'args' contient les arguments en ligne de commande
    // Utilisez un boucle foreach pour parcourir les arguments
    foreach (string arg in args)
    {
        Console.WriteLine(arg);
    }
}
```
Output:
```
> MonProgramme.exe argument1 argument2

argument1
argument2
```

## Plongée en profondeur

Lire les arguments en ligne de commande peut sembler simple, mais il y a quelques détails à prendre en compte pour bien le faire. Tout d'abord, sachez que la variable 'args' est de type 'string[]', c'est-à-dire un tableau de chaînes de caractères. Vous pouvez donc utiliser toutes les méthodes et propriétés d'un tableau pour manipuler vos arguments.

De plus, gardez à l'esprit qu'il est possible de passer des arguments avec des guillemets, ce qui peut entraîner des problèmes si vous n'y faites pas attention. Par exemple, si votre argument contient un espace, il sera considéré comme deux arguments distincts.

Enfin, il est important de vérifier que les arguments ont été fournis dans le bon ordre et qu'ils correspondent bien à ceux attendus par votre programme. Une erreur peut facilement survenir si un utilisateur entre des arguments de manière incorrecte.

## Voir aussi

- [Documentation officielle sur les arguments de ligne de commande en C#](https://docs.microsoft.com/fr-fr/dotnet/csharp/programming-guide/main-and-command-args/)
- [Article sur les bonnes pratiques en programmation en ligne de commande avec C#](https://lostechies.com/seanbiefeld/2013/12/29/better-command-line-parsing-in-net/)

Félicitations, vous savez maintenant comment lire efficacement les arguments en ligne de commande en C#. N'hésitez pas à consulter ces sources pour en apprendre davantage sur les meilleures pratiques en la matière.