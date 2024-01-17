---
title:                "Utiliser les expressions régulières"
html_title:           "C++: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi les programmeurs l'utilisent-ils?

Les expressions régulières sont une méthode puissante pour rechercher des motifs dans du texte. Les programmeurs les utilisent souvent pour valider des données d'entrée, extraire des informations spécifiques et effectuer des opérations de recherche et de remplacement.

# Comment faire:

Voici un exemple de code en C++ utilisant des expressions régulières pour rechercher un motif dans une chaîne de caractères :

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
  string s = "Bonjour le monde !";
  // Recherche du motif "monde" dans la chaîne de caractères
  regex reg("monde");
  // Vérification si le motif existe dans la chaîne
  if (regex_search(s, reg)) {
    cout << "Le motif a été trouvé !" << endl;
  }
  return 0;
}
```

La sortie du code ci-dessus sera :

```
Le motif a été trouvé !
```

# Approfondissement :

Les expressions régulières ont été inventées dans les années 1950 par le mathématicien Stephen Kleene. À l'origine, elles étaient utilisées pour définir des langages formels. Depuis, elles sont devenues un outil essentiel pour les programmeurs, avec des alternatives telles que les fonctions de manipulation de chaînes de caractères.

Pour implémenter des expressions régulières en C++, vous aurez besoin de la bibliothèque standard <regex>. Cette bibliothèque comprend des classes et des fonctions qui vous permettent de créer et de manipuler des expressions régulières.

# Voir aussi :

- [Tutoriel sur les expressions régulières en C++](https://www.cplusplus.com/reference/regex/)
- [Documentation complète de la bibliothèque <regex>](https://cplusplus.com/reference/regex/)
- [Histoire des expressions régulières](https://en.wikipedia.org/wiki/Regular_expression)