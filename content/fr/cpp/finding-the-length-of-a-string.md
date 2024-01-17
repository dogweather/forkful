---
title:                "Trouver la longueur d'une chaîne."
html_title:           "C++: Trouver la longueur d'une chaîne."
simple_title:         "Trouver la longueur d'une chaîne."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est et pourquoi les programmeurs le font-ils?

Trouver la longueur d'une chaîne de caractères est essentiel pour de nombreuses tâches de programmation. En termes simples, cela signifie compter le nombre de caractères dans une chaîne. Les programmeurs le font pour traiter et manipuler des données, comme pour valider les entrées d'utilisateurs ou pour boucler à travers une chaîne de caractères.

Comment le faire:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Déclaration d'une chaîne de caractères
    string message = "Bonjour tout le monde!";
    
    // Utilisation de la méthode length pour trouver la longueur de la chaîne
    int longueur = message.length();
    
    // Affichage du résultat
    cout << "La longueur de la chaîne '" << message << "' est: " << longueur << endl;
    
    return 0;
}
```

Résultat:

```
La longueur de la chaîne 'Bonjour tout le monde!' est: 21
```

Plongée dans la technique:

Trouver la longueur d'une chaîne de caractères peut sembler simple, mais cela a pris du temps pour être développé et amélioré dans les langages de programmation. Les premières langues, comme le langage d'assemblage, n'avaient pas de fonction native pour calculer la longueur d'une chaîne. Les programmeurs devaient donc utiliser des astuces pour la trouver. De nos jours, les langages de programmation modernes ont des fonctions intégrées pour cette tâche.

Alternatives:

En plus de l'utilisation de la méthode length comme dans l'exemple précédent, il existe d'autres moyens de trouver la longueur d'une chaîne de caractères. Par exemple, en C, on peut utiliser la fonction strlen() de la bibliothèque standard. En Python, la fonction len() peut être utilisée.

Liens supplémentaires:

Pour en savoir plus sur la méthode length ou sur d'autres façons de trouver la longueur d'une chaîne de caractères, vous pouvez consulter les ressources suivantes :

- [Documentation du langage C++](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [Documentation du langage C](https://en.cppreference.com/w/cpp/string/byte/strlen)
- [Documentation du langage Python](https://docs.python.org/3/library/functions.html#len)
- [Historique de la recherche et du développement des méthodes pour trouver la longueur d'une chaîne de caractères](https://en.wikipedia.org/wiki/String_length)