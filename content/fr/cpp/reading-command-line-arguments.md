---
title:                "Lecture des arguments de ligne de commande"
html_title:           "C++: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

La lecture des arguments de ligne de commande est une compétence cruciale pour tout programmeur en C++. Elle nous permet de fournir des paramètres à notre programme au moment de son exécution, ce qui le rend personnalisable et plus polyvalent. Grâce à cette capacité, nous pouvons modifier le comportement de notre programme sans avoir à recompiler une nouvelle version à chaque fois.

## Comment faire:

Voici un exemple simple pour vous montrer comment lire les arguments de ligne de commande en C++:

```C++
#include <iostream>

int main(int argc, char *argv[]) {
  std::cout << "Le nombre d'arguments est: " << argc << std::endl;
  for(int i = 0; i < argc; i++) {
    std::cout << "Argument " << i << ": " << argv[i] << std::endl;
  }
  return 0;
}
```

Supposons que notre programme s'appelle "myprogram.exe" et que nous l'exécutons avec deux arguments: "param1" et "param2". Voici le résultat que nous obtiendrons:

```
Le nombre d'arguments est: 3
Argument 0: myprogram.exe
Argument 1: param1
Argument 2: param2
```

## Plongée en profondeur:

La lecture des arguments de ligne de commande n'est pas une nouveauté dans le monde de la programmation. Cependant, cette capacité est extrêmement utile pour rendre nos programmes plus dynamiques et personnalisables. Il existe également des alternatives à la lecture des arguments de ligne de commande, comme l'utilisation de variables d'environnement ou la création d'interfaces graphiques.

Pour implémenter cette fonctionnalité en C++, nous devons utiliser le paramètre "argc" qui représente le nombre d'arguments passés lors de l'exécution du programme, et le tableau "argv" qui contient les arguments eux-mêmes. Les arguments peuvent être de n'importe quel type, y compris des chaînes de caractères, des nombres ou même des expressions régulières.

## Voir aussi:

Si vous souhaitez en savoir plus sur la lecture des arguments de ligne de commande en C++, vous pouvez consulter ces sources:

- [Documentation C++ sur les arguments de ligne de commande](https://en.cppreference.com/w/cpp/language/main_function)
- [Tutoriel vidéo sur la lecture des arguments de ligne de commande en C++](https://www.youtube.com/watch?v=S7UYoFnlz-g)
- [Un article de blog avec des exemples pratiques de lecture des arguments de ligne de commande en C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)