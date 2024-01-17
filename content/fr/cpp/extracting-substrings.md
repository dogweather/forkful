---
title:                "Extraction de sous-chaînes"
html_title:           "C++: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faisons-nous?

L'extraction de sous-chaînes est une technique de programmation permettant de récupérer une partie spécifique d'une chaîne de caractères. Cela peut être utile lors de la manipulation de données volumineuses ou lors de la recherche de motifs spécifiques dans une chaîne de texte. Les programmeurs utilisent cette méthode pour simplifier leur code et le rendre plus efficace.

## Comment le faire:

Voici un exemple simple de code en C ++ pour extraire une sous-chaîne à partir d'une chaîne de caractères:

```
#include <iostream>
#include <string>

using namespace std;

int main() {
    string chaine = "Bonjour les amis!";
    string sous_chaine = chaine.substr(7, 5); // extrait les caractères à partir de l'index 7 pour une longueur de 5 caractères
    cout << sous_chaine << endl; // affiche "les a"
    return 0;
}
```

## Plongée en profondeur:

L'extraction de sous-chaînes est une fonctionnalité couramment utilisée parmi les langages de programmation modernes. Elle a été introduite pour la première fois en 1972 dans le langage Snobol4. Il existe également d'autres méthodes pour extraire des sous-chaînes, telles que l'utilisation de pointeurs ou de boucles. Cependant, la méthode `substr()` en C ++ est considérée comme plus simple et plus efficace. Elle est largement utilisée dans des tâches de traitement de texte, telles que la recherche et le remplacement de mots.

## Voir aussi:

Pour plus d'informations sur l'extraction de sous-chaînes en C ++, vous pouvez consulter les sources suivantes:

- [Documentation officielle de C++ pour la fonction `substr()`](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutoriel pour l'extraction de sous-chaînes en C++](https://www.geeksforgeeks.org/substring-in-cpp/)
- [Exemples pratiques d'utilisation de la fonction `substr()` en C++](https://www.techiedelight.com/extract-substring-string-cpp/)