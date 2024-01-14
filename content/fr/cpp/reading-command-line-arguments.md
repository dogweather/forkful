---
title:    "C++: Lecture des arguments de ligne de commande"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur C++, vous avez probablement déjà entendu parler des arguments de ligne de commande, mais savez-vous vraiment pourquoi ils sont importants ? Les arguments de ligne de commande sont utilisés pour fournir des informations à un programme lors de son exécution. Cela peut inclure des options de configuration, des fichiers à traiter ou des données d'entrée. En fait, les arguments de ligne de commande peuvent grandement améliorer l'efficacité et la flexibilité de votre code.

## Comment Faire

Pour lire les arguments de ligne de commande en C++, nous utilisons la fonction `main()`. Cette fonction prend deux paramètres : `argc` et `argv`. `argc` représente le nombre total d'arguments passés au programme, tandis que `argv` est un tableau de chaînes de caractères contenant les arguments eux-mêmes.

Voici un exemple simple de lecture et d'affichage des arguments de ligne de commande :

```C++
#include <iostream>

int main(int argc, char* argv[])
{
    std::cout << "Nombre d'arguments : " << argc << std::endl;
    for (int i = 0; i < argc; i++)
    {
        std::cout << "Argument n°" << i << " : " << argv[i] << std::endl;
    }
    return 0;
}
```

Si nous exécutons ce programme avec les arguments `./programme Arg1 Arg2`, nous obtiendrons la sortie suivante :

```
Nombre d'arguments : 3
Argument n°0 : ./programme
Argument n°1 : Arg1
Argument n°2 : Arg2
```

Nous pouvons également utiliser des arguments de ligne de commande pour créer des fonctionnalités plus avancées, comme l'ouverture et la lecture de fichiers spécifiés par l'utilisateur.

## Plongée Profonde

La fonction `main()` en C++ peut également prendre un troisième paramètre, `envp`, qui représente les variables d'environnement du système. Ces variables peuvent être utiles pour récupérer des informations supplémentaires, telles que le nom d'utilisateur ou le répertoire de travail actuel.

En outre, il existe des bibliothèques externes disponibles pour faciliter la manipulation des arguments de ligne de commande en C++. Par exemple, la bibliothèque [Boost.Program_options](https://www.boost.org/doc/libs/1_77_0/doc/html/program_options.html) fournit des fonctions pratiques pour lire et analyser les arguments avec une syntaxe facile à utiliser.

## Voir Aussi

- [Documentation sur les arguments de ligne de commande en C++](https://en.cppreference.com/w/cpp/language/main_function)
- [Guide d'utilisation de la bibliothèque Boost.Program_options](https://www.boost.org/doc/libs/1_77_0/doc/html/program_options/tutorial.html)
- [Exemples de manipulation des arguments de ligne de commande en C++](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)