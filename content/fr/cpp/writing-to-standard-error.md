---
title:                "Écrire sur le flux d'erreurs standard"
html_title:           "C++: Écrire sur le flux d'erreurs standard"
simple_title:         "Écrire sur le flux d'erreurs standard"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Pourquoi écrire vers l'erreur standard?

Il peut y avoir plusieurs raisons pour lesquelles un développeur souhaiterait écrire vers l'erreur standard dans son code C++. Voici quelques-unes des raisons les plus courantes:

- Débogage: En écrivant vers l'erreur standard, les erreurs et les avertissements peuvent être affichés directement dans la console, ce qui facilite le processus de débogage.
- Communication avec l'utilisateur: Lorsque vous écrivez un programme en ligne de commande, il peut être utile d'afficher des messages directement dans la console pour communiquer avec l'utilisateur.

Comment le faire:

Pour écrire vers l'erreur standard en C++, vous pouvez utiliser la fonction ```std::cerr``` et utiliser l'opérateur de flux ```<<``` pour afficher les messages.

Voici un exemple de code qui écrit un message d'erreur à l'aide de ```std::cerr```:

```C++
#include <iostream>

int main()
{
    int num = -10;

    // Vérifier si le numéro est négatif
    if (num < 0) {
        // Écrire un message d'erreur vers l'erreur standard
        std::cerr << "ERREUR: Le numéro ne peut pas être négatif." << std::endl;
    }

    return 0;
}
```

Lorsque vous exécutez ce code, vous verrez le message d'erreur s'afficher dans votre console:

```
ERREUR: Le numéro ne peut pas être négatif.
```

Profondeur:

Maintenant que vous savez comment utiliser ```std::cerr``` pour écrire vers l'erreur standard, il est important de comprendre comment cela fonctionne réellement.

En C++, il existe trois flux standard prédéfinis: ```std::cin``` pour les entrées de l'utilisateur, ```std::cout``` pour les sorties standard et ```std::cerr``` pour les erreurs.

Par défaut, ces flux sont tous liés à la console, mais vous pouvez utiliser la fonction ```std::freopen()``` pour rediriger ces flux vers un fichier si nécessaire.

Il est également important de noter que lorsqu'une erreur est écrite vers l'erreur standard, le programme se poursuit normalement. Contrairement à ```std::cout```, qui est associé à la sortie standard, écrire vers l'erreur standard n'affecte pas la valeur de retour du programme.

Veuillez noter que l'utilisation excessive d'écriture vers l'erreur standard peut entraîner une performance réduite de votre programme, il est donc important de l'utiliser avec parcimonie.

Voir aussi:

- [Documentation C++ sur les flux standard](https://en.cppreference.com/w/cpp/io)
- [Guide de débogage en C++](https://www.learncpp.com/cpp-tutorial/debugging-tips-tricks/)
- [Exemples de redirection de flux en C++](https://www.geeksforgeeks.org/redirecting-stdout-to-a-file-in-c/)





## Voir aussi:

- [Documentation C++ sur les flux standard](https://en.cppreference.com/w/cpp/io)
- [Guide de débogage en C++](https://www.learncpp.com/cpp-tutorial/debugging-tips-tricks/)
- [Exemples de redirection de flux en C++](https://www.geeksforgeeks.org/redirecting-stdout-to-a-file-in-c/)