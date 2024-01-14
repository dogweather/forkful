---
title:    "C++: Affichage des résultats de débogage"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous programmez en C++, vous vous retrouvez souvent confronté à des bogues ou des erreurs qui peuvent être difficiles à trouver et à résoudre. C'est ici qu'entre en jeu l'impression de sortie de débogage. En ajoutant des instructions d'impression de débogage à votre code, vous pouvez facilement suivre l'exécution de votre programme et trouver les erreurs plus rapidement.

# Comment Faire

Pour afficher des informations de débogage, nous utilisons la fonction `cout` de la bibliothèque standard de C++. Cela nous permet d'imprimer des valeurs de variables, des messages ou tout autre contenu dans la console pendant l'exécution du programme.

Voici un exemple de code C++ utilisant `cout` pour imprimer le contenu d'une variable:

```C++
#include <iostream>

int main() {
    int age = 25;
    std::cout << "Mon age est: " << age << "\n";
    return 0;
}
```

La sortie de ce programme sera:

```
Mon age est: 25
```

Nous pouvons également utiliser `cout` pour imprimer des messages explicatifs pour nous aider à comprendre l'exécution de notre programme. Par exemple:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 2;
    std::cout << "La somme de " << a << " et " << b << " est: " << (a + b) << "\n";
    return 0;
}
```

La sortie de ce programme sera:

```
La somme de 5 et 2 est: 7
```

# Approfondissement

Outre l'impression de valeurs de variables et de messages, nous pouvons également utiliser `cout` pour afficher des informations sur l'état de l'exécution de notre programme. Par exemple, nous pouvons imprimer le numéro de ligne où nous utilisons `cout` pour suivre l'ordre d'exécution de notre code.

De plus, nous pouvons utiliser l'opérateur `<<` pour concaténer plusieurs variables ou messages dans une seule instruction `cout`. Cela peut être utile lorsque nous souhaitons suivre plusieurs informations en même temps.

L'impression de sortie de débogage peut également être utile lorsque nous travaillons avec des boucles ou des fonctions récursives, où nous pouvons imprimer les valeurs de variables à chaque itération pour suivre le flux d'exécution de notre code.

# Voir Aussi

- [C++ Debugging Techniques](https://www.tutorialspoint.com/cplusplus/cpp_debugging_techniques.htm)
- [Using cout for debugging](https://www.learncpp.com/cpp-tutorial/27-using-cout-for-debugging/)
- [Debugging C++ Code Using GDB](https://www.guru99.com/c-dbg-debugging.html)