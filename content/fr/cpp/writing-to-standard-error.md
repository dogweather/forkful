---
title:    "C++: Écrire vers l'erreur standard"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture vers le standard error est un outil important pour les programmeurs en C++. Cela permet de signaler des erreurs ou des informations de débogage importantes pendant l'exécution d'un programme en dehors de la sortie standard. Cela peut être utile pour les développeurs lorsqu'ils déboguent leur code ou pour les utilisateurs lorsqu'ils exécutent des programmes en ligne de commande.

## Comment faire

Pour écrire vers le standard error en C++, il suffit d'utiliser la fonction "std::cerr" qui est disponible dans la bibliothèque standard <iostream>. Voici un exemple de code qui affiche un message d'erreur et un message de débogage :

```C++
#include <iostream>

int main() {
    std::cerr << "Une erreur s'est produite !" << std::endl;
    std::cerr << "Le numéro de ligne est : " << __LINE__ << std::endl;
    return 0;
}
```

La sortie de ce programme sera :

```
Une erreur s'est produite !
Le numéro de ligne est : 5
```

Comme vous pouvez le voir, le premier message est affiché sur une ligne séparée de la sortie standard et est précédé par "[Erreur]" pour indiquer qu'il s'agit d'un message d'erreur.

## Plongée en profondeur

Il est également possible d'écrire vers le standard error en utilisant la classe "std::basic_ostream" qui est la classe de base pour les objets de flux tels que std::cout et std::cerr. Cela permet une plus grande flexibilité dans le formatage des messages d'erreur et de débogage. Par exemple :

```C++
#include <iostream>

int main() {
    std::basic_ostream<char> my_error_stream(std::cerr.rdbuf());
    my_error_stream << "[ERREUR] Une erreur s'est produite !" << std::endl;
    return 0;
}
```

La sortie de ce programme sera :

```
[ERREUR] Une erreur s'est produite !
```

Il est également important de noter que la fonction "std::cerr" est synchronisée par défaut avec la sortie standard, ce qui signifie que les messages d'erreur apparaîtront immédiatement dans le terminal, même si le programme se bloque ou se termine prématurément.

## Voir aussi

- [Documentation de std::cerr en français](https://fr.cppreference.com/w/cpp/io/cerr)
- [Article sur l'utilisation de std::cerr en C++](https://www.tutorialspoint.com/cplusplus/cpp_error_handling.htm)
- [Exemple pratique d'utilisation de std::cerr pour le débogage](https://www.guru99.com/c-plus-plus-error-handling.html)