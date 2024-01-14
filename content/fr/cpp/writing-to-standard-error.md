---
title:                "C++: Écrire vers l'erreur standard"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi écrire vers la sortie d'erreur standard en C++

Ecrire vers la sortie d'erreur standard est une pratique courante dans la programmation en C++. Cela permet d'afficher des messages d'erreur ou des informations de débogage pendant l'exécution d'un programme. Dans cet article, nous vous expliquerons pourquoi il est important d'utiliser cette méthode et comment le faire efficacement.

## Comment utiliser la sortie d'erreur standard

Pour écrire vers la sortie d'erreur standard en C++, il suffit d'utiliser la fonction `cerr` avec l'opérateur "<<" pour afficher ce que vous souhaitez. Voici un exemple de code:

```C++
#include <iostream>

using namespace std;

int main() {
    double a = 10.5;
    double b = 0;

    if(b == 0) {
        cerr << "Erreur: la variable b ne peut pas être égale à 0." << endl;
    } else {
        double resultat = a / b;
        cout << "Le résultat est: " << resultat << endl;
    }

    return 0;
}
```

Dans cet exemple, nous tentons de diviser la variable `a` par la variable `b`. Si `b` est égal à 0, nous afficherons un message d'erreur vers la sortie d'erreur standard à l'aide de la fonction `cerr`. Sinon, nous afficherons le résultat vers la sortie standard `cout`.

Lors de l'exécution de ce programme, le résultat sera le suivant:

```sh
Erreur: la variable b ne peut pas être égale à 0.
```

Cela montre l'importance d'utiliser la sortie d'erreur standard pour informer l'utilisateur en cas d'erreur.

## Approfondissement

En utilisant la sortie d'erreur standard, vous avez la possibilité d'afficher des informations de débogage qui peuvent vous aider à comprendre le comportement de votre programme. Vous pouvez également utiliser des indicateurs, tels que `cerr << "Ici" << endl;`, pour savoir à quel endroit de votre code la sortie d'erreur a été déclenchée.

Il est également important de savoir que la sortie d'erreur standard n'a pas besoin d'être redirigée pour être affichée à l'utilisateur. Contrairement à la sortie standard, qui peut être redirigée vers un fichier ou un flux, la sortie d'erreur standard est toujours affichée sur la console.

# Voir aussi

- [Documentation - `cerr` en Cplusplus.com](https://www.cplusplus.com/reference/iostream/cerr/)
- [Guide de débogage en C++, partie 1 - Sortie d'erreur standard et d'autres techniques](https://www.codeproject.com/articles/1029838/debugging-in-cplusplus-part-1-standard-error-outpu)