---
title:    "C++: Lecture des arguments de la ligne de commande"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes nouveau dans le monde de la programmation, vous avez peut-être entendu parler de la lecture des arguments de la ligne de commande. Mais pourquoi devriez-vous vous en soucier ? Eh bien, la lecture des arguments de la ligne de commande vous permet de fournir des paramètres à votre programme lors de son exécution, ce qui peut être très utile lors de l'automatisation de certaines tâches ou de la personnalisation de votre programme en fonction des besoins spécifiques de l'utilisateur.

# Comment faire

Pour lire les arguments de la ligne de commande en C++, nous pouvons utiliser les paramètres de fonction main() qui nous sont automatiquement fournis. Voici un exemple de code qui affiche les arguments fournis lors de l'exécution du programme :

```C++
#include <iostream>

int main(int argc, char** argv) {
    // Le premier argument (argv[0]) est le nom du programme lui-même
    std::cout << "Nom du programme : " << argv[0] << std::endl;

    // Nous pouvons parcourir les autres arguments en utilisant une boucle for
    for (int i = 1; i < argc; i++) {
        std::cout << "Argument " << i << " : " << argv[i] << std::endl;
    }

    return 0;
}
```

Voici un exemple de sortie pour un programme appelé "mon_programme" avec les arguments "bonjour" et "monde" :

```
Nom du programme : mon_programme
Argument 1 : bonjour
Argument 2 : monde
```

# Plongée en profondeur

Maintenant que nous savons comment lire les arguments de la ligne de commande, il est important de noter que ces arguments sont traités comme des chaînes de caractères (ou des tableaux de caractères) dans notre programme. Cela signifie que si nous voulons utiliser ces arguments comme des entiers ou des nombres à virgule flottante, nous devrons les convertir en utilisant des fonctions appropriées telles que `stoi()` et `stof()`. De plus, il est possible de passer des drapeaux aux arguments de la ligne de commande en utilisant le signe '-' avant un argument, ce qui peut être utile pour activer certaines fonctionnalités dans notre programme.

# Voir aussi

- [Documentation sur les arguments de la ligne de commande en C++](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [Introduction aux arguments de la ligne de commande en C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)