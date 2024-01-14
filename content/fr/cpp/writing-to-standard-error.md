---
title:                "C++: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire sur la sortie d'erreur standard peut sembler être un sujet banal, mais c'est en fait une pratique très importante en programmation. Cela permet de mieux gérer les erreurs et de déboguer efficacement votre code.

## Comment faire

Pour écrire sur la sortie d'erreur standard en C++, vous pouvez utiliser la bibliothèque standard `iostream` avec la fonction `std::cerr`. Voici un exemple de code :

```C++
#include <iostream>

int main() {
    // code qui génère une erreur
    std::cerr << "Erreur : impossible d'ouvrir le fichier" << std::endl;
    return 0;
}
```

Lors de l'exécution de ce code, vous obtiendrez un message d'erreur clair sur la sortie d'erreur standard.

```
Erreur : impossible d'ouvrir le fichier
```

Vous pouvez également utiliser `std::cerr` pour afficher des messages de débogage en ajoutant des informations supplémentaires, comme dans l'exemple suivant :

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    std::cerr << "a = " << a << ", b = " << b << std::endl;
    std::cerr << "Le résultat de a / b est : " << a / b << std::endl;
    return 0;
}
```

La sortie d'erreur standard indiquera alors :

```
a = 5, b = 0
Le résultat de a / b est : Erreur de segmentation (core dumped)
```

Ceci peut être très utile pour déterminer où votre code a rencontré une erreur et ainsi faciliter le processus de débogage.

## Plongée en profondeur

Il peut parfois être frustrant de trouver la source d'une erreur dans votre code. C'est pourquoi il est important de bien comprendre comment utiliser la sortie d'erreur standard pour afficher des messages précis et utiles.

Il est également possible de rediriger la sortie d'erreur standard vers un fichier en utilisant la commande `2>` en ligne de commande. Ainsi, vous pourrez enregistrer les messages d'erreur pour les consulter ultérieurement. De plus, vous pouvez utiliser la directive `#define` pour définir une macro qui redirigera tous les appels à `std::cerr` vers un fichier, plutôt que de les écrire un par un dans votre code.

## Voir aussi

- [Documentation de la bibliothèque standard C++ : iostream](https://fr.cppreference.com/w/cpp/io/c/err)
- [Tutorial sur la sortie d'erreur standard en C++](https://www.learncpp.com/cpp-tutorial/6-12a-output-with-stdcerr/)
- [Article sur la redirection de la sortie d'erreur standard en C++](https://medium.com/@yuvrajwadhwani/redirection-of-output-to-file-and-stdout-in-c-2ad0cf559313)