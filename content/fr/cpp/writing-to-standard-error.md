---
title:                "Écrire vers l'erreur standard"
html_title:           "C++: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

Ecrire vers la sortie standard d'erreur est un moyen pour les programmeurs d'afficher des erreurs ou des messages de débogage à l'utilisateur ou à d'autres parties du programme. Cela permet d'améliorer la lisibilité et la maintenance du code en séparant les messages d'erreur des sorties régulières.

# Comment faire:

Voici un exemple de code en C ++ montrant comment écrire vers la sortie standard d'erreur :

```
#include <iostream>
#include <cstdlib>

using namespace std;

int main() {
    cerr << "Ceci est un message d'erreur!" << endl;
    return EXIT_FAILURE;
}
```

Voici le résultat lorsque vous exécutez le programme :

```
Ceci est un message d'erreur!
```

# Plongée en profondeur:

## Contexte historique:

L'écriture vers la sortie standard d'erreur est une pratique courante dans la programmation depuis le développement des premiers compilateurs. Les langages de programmation tels que le C et le C ++ ont des flux distincts pour les sorties régulières et les erreurs, ce qui a conduit à l'utilisation de «cout» pour les sorties et «cerr» pour les erreurs.

## Alternatives:

Une alternative courante à l'écriture vers la sortie standard d'erreur est d'utiliser des exceptions. Les exceptions sont des objets spéciaux qui peuvent être levés (throw) lorsqu'une erreur se produit, et sont mieux adaptées aux situations où l'erreur peut être traitée à différents niveaux du programme.

## Détails de l'implémentation:

Pour écrire vers la sortie standard d'erreur en C ++, on utilise l'objet «cerr» de la classe «ostream», qui est défini dans la bibliothèque standard «iostream». La fonction «std::exit» est utilisée pour quitter le programme avec un code d'erreur spécifique.

# Voir aussi:

Pour en savoir plus sur l'écriture vers la sortie standard d'erreur en C ++, voici quelques liens utiles :

- [Documentation officielle de la bibliothèque standard C ++](https://en.cppreference.com/w/)
- [Tutoriel sur les exceptions en C ++](https://www.learncpp.com/cpp-tutorial/exceptions-throwing-exceptions-and-catching-exceptions/)