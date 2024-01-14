---
title:                "C#: Analyse des arguments de ligne de commande"
simple_title:         "Analyse des arguments de ligne de commande"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes programmeur en C#, il est très probable que vous ayez déjà entendu parler des arguments de ligne de commande. Mais peut-être que vous ne comprenez pas tout à fait pourquoi ils sont utiles et pourquoi vous devriez apprendre à les utiliser. Dans cet article, je vais vous expliquer pourquoi il est important de pouvoir lire les arguments de ligne de commande dans vos programmes.

## Comment faire

Pour commencer, jetons un coup d'œil à un exemple de code qui utilise des arguments de ligne de commande en C#. Nous allons créer un programme simple qui prend un argument de ligne de commande et l'affiche à l'écran. Voici le code :

```C#
using System;

namespace CommandLineArgs
{
    class Program
    {
        static void Main(string[] args)
        {
            // Vérifie si l'argument de ligne de commande a été fourni
            if (args.Length > 0)
            {
                // Affiche le premier argument à l'écran
                Console.WriteLine("L'argument fourni est : " + args[0]);
            }
            else
            {
                Console.WriteLine("Aucun argument fourni !");
            }
        }
    }
}
```

Ensuite, nous allons compiler et exécuter le code avec différents arguments de ligne de commande :

```
> csc Program.cs
> Program.exe
Aucun argument fourni !

> Program.exe Bonjour
L'argument fourni est : Bonjour
```

Comme vous pouvez le voir, lorsque nous exécutons le programme sans argument, il affiche simplement un message indiquant qu'aucun argument n'a été fourni. Mais lorsque nous lui donnons un argument, il l'affiche à l'écran.

## Plongée en profondeur

Maintenant que vous savez comment utiliser les arguments de ligne de commande, vous pourriez vous demander à quoi cela peut vous servir. Eh bien, les arguments de ligne de commande peuvent être très utiles lorsque vous souhaitez que votre programme soit plus flexible. Par exemple, vous pourriez vouloir que votre programme puisse agir différemment en fonction de l'argument fourni. Cela peut être particulièrement utile dans les scripts ou les programmes en ligne de commande.

Vous pouvez également utiliser les arguments de ligne de commande pour passer des informations spécifiques à votre programme, telles que des paramètres de configuration ou des chemins de fichiers. Cela peut vous éviter d'avoir à modifier votre code chaque fois que vous devez changer ces valeurs.

Enfin, les arguments de ligne de commande peuvent également être utilisés pour déboguer votre programme. Par exemple, si vous rencontrez un problème avec votre programme, vous pouvez utiliser un argument de ligne de commande pour activer un mode de débogage et afficher plus d'informations sur ce qui se passe dans votre code.

## Voir aussi

Maintenant que vous avez compris comment utiliser les arguments de ligne de commande en C#, vous pouvez les incorporer dans vos programmes pour les rendre plus polyvalents et pratiques. Pour en savoir plus sur les arguments de ligne de commande, voici quelques liens utiles :

- [Documentation officielle de Microsoft sur les arguments de ligne de commande en C#](https://docs.microsoft.com/fr-fr/archive/msdn-magazine/2014/december/command-line-parsing-with-system-commandlineto)
- [Un tutoriel vidéo sur les arguments de ligne de commande en C#](https://www.youtube.com/watch?v=fnYxuI-JxnI)
- [Un article sur l'utilisation des arguments de ligne de commande pour personnaliser vos programmes en C#](https://www.codeproject.com/Articles/3111/Command-Line-Arguments-in-C-)

N'hésitez pas à explorer davantage pour découvrir toutes les possibilités qu'offrent les arguments de ligne de commande en C#. Bonne programmation !