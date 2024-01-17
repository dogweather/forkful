---
title:                "Écrire vers la sortie d'erreur standard"
html_title:           "C: Écrire vers la sortie d'erreur standard"
simple_title:         "Écrire vers la sortie d'erreur standard"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Ecrire vers la sortie standard error est un moyen pour les programmeurs de fournir des messages d'erreurs et des informations de débogage à l'utilisateur lors de l'exécution d'un programme. Cela permet de séparer les messages d'erreurs des messages de sortie normaux et ainsi, de simplifier la compréhension et la résolution des problèmes.

## Comment faire:

Voici un exemple de code qui écrit un message d'erreur vers la sortie standard error en utilisant la fonction ```fprintf()```:

```
#include <stdio.h>

int main() {

    fprintf(stderr, "Erreur: impossible d'ouvrir le fichier.");
    return 0;
}
```

Cela produirait la sortie suivante:

```
Erreur: impossible d'ouvrir le fichier.
```

## Plongée en profondeur:

Avant l'utilisation du standard error, les messages d'erreurs étaient souvent mélangés avec la sortie normale du programme, ce qui pouvait rendre la résolution des problèmes plus difficile. L'utilisation de la sortie standard error permet également aux développeurs de rediriger les messages d'erreurs vers un fichier pour un examen ultérieur.

Il existe d'autres moyens d'afficher des messages d'erreurs, tels que l'utilisation de la fonction ```perror()``` ou l'émission de codes d'erreurs spécifiques avec la fonction ```exit()```. De plus, il est important de gérer correctement les erreurs pour garantir une bonne expérience utilisateur.

L'implémentation de l'écriture vers la sortie standard error peut varier selon le système d'exploitation utilisé.

## Voir aussi:

- [Documentation de la fonction fprintf() en C](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [Guide pour la gestion des erreurs en C](https://www.guru99.com/c-error-handling.html)
- [Plus d'informations sur la redirection de la sortie en C](https://www.tldp.org/LDP/abs/html/io-redirection.html)