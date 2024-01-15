---
title:                "Lecture des arguments en ligne de commande"
html_title:           "C++: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà utilisé des lignes de commande pour exécuter un programme, vous avez probablement vu des arguments être passés après le nom du fichier exécutable. Ces arguments permettent à un utilisateur de personnaliser l'exécution du programme en modifiant ses paramètres. Dans cet article, nous allons expliquer comment lire et utiliser ces arguments en C++, afin que vous puissiez maximiser l'utilité de vos programmes en ligne de commande.

## Comment Faire

La lecture des arguments de ligne de commande en C++ est un processus assez simple. Tout d'abord, vous devez utiliser les paramètres de la fonction main pour récupérer les arguments. La fonction main a deux paramètres, argc et argv. Le premier représente le nombre total d'arguments passés, tandis que le second est un tableau de chaînes de caractères contenant les arguments eux-mêmes.

```C++
int main(int argc, char* argv[]) {
  // Utilisez les paramètres argc et argv pour lire les arguments.
}
```

Pour accéder à un argument spécifique, vous pouvez utiliser son indice dans le tableau argv. Par exemple, pour récupérer le premier argument, vous pouvez utiliser ```argv[1]```. Notez que le premier argument (```argv[0]```) représente toujours le nom du fichier exécutable lui-même.

Une fois que vous avez récupéré un argument, vous pouvez le convertir en un type de données approprié pour l'utiliser dans votre programme. Par exemple, vous pouvez utiliser la fonction ```std::stoi()``` pour convertir un argument en entier.

Enfin, n'oubliez pas de gérer les erreurs potentielles en vérifiant le nombre d'arguments et en utilisant des structures de contrôle comme ```if``` ou ```switch``` pour traiter différents scénarios d'utilisation des arguments.

## Plongée Profonde

Il existe de nombreuses façons de tirer parti des arguments de ligne de commande en C++. Vous pouvez par exemple les utiliser pour:

- Passer des options de configuration à votre programme.
- Fournir des entrées de données au programme.
- Contrôler le comportement du programme en fonction des arguments fournis.

Vous pouvez également utiliser des bibliothèques tierces comme ```Boost.Program_options``` pour simplifier davantage la lecture et l'utilisation des arguments de ligne de commande.

## Voir Aussi

- [Documentation de la fonction main en C++](https://fr.cppreference.com/w/cpp/language/main_function)
- [Tutoriel sur les arguments de ligne de commande en C++](https://www.learncpp.com/cpp-tutorial/arguments-command-line/)
- [Boost.Program_options](https://www.boost.org/doc/libs/1_76_0/doc/html/program_options.html) (documentation officielle)