---
title:    "C++: Recherche et remplacement de texte."
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des opérations courantes en programmation, qui permettent de modifier rapidement et efficacement du code ou du texte. Cela peut être utile lors de la correction d'erreurs ou lors de l'optimisation de votre code.

## Comment faire

La recherche et le remplacement de texte peuvent être réalisés en utilisant la fonction `replace()` de la bibliothèque standard de C++. Voici un exemple de code en utilisant cette fonction :

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string texte = "Bonjour le monde !";
    cout << "Texte initial : " << texte << endl;
    texte.replace(8, 2, "soir");
    cout << "Texte modifié : " << texte << endl;
    return 0;
}
```

Cet exemple va remplacer les deux caractères à partir de la 8ème position dans la chaîne de caractères `texte` par le mot "soir". Le résultat de l'exécution de ce code sera le texte suivant :

```
Texte initial : Bonjour le monde !
Texte modifié : Bonsoir le monde !
```

Il est important de noter que la fonction `replace()` modifie directement la chaîne de caractères initiale, elle ne renvoie pas une nouvelle chaîne. Ainsi, si vous voulez conserver la chaîne originale, vous devrez la copier dans une autre variable avant d'appliquer la fonction `replace()`.

## Plongée en profondeur

En plus de la fonction `replace()`, la bibliothèque standard de C++ offre d'autres possibilités pour effectuer une recherche et un remplacement de texte. Par exemple, vous pouvez utiliser les fonctions `find()` et `substr()` pour localiser et extraire une partie d'une chaîne de caractères, puis la remplacer avec `replace()`. Il existe également des bibliothèques et des outils externes qui peuvent vous aider à effectuer des recherches et des remplacements plus complexes, comme l'expression régulière de la bibliothèque `<regex>`.

N'hésitez pas à explorer et à expérimenter avec ces différentes options pour trouver celle qui est la plus adaptée à vos besoins.

## Voir aussi

- Documentation de la fonction `replace()` de la bibliothèque standard de C++ : [https://en.cppreference.com/w/cpp/string/basic_string/replace](https://en.cppreference.com/w/cpp/string/basic_string/replace)
- Tutoriel sur les expressions régulières en C++ : [https://www.regular-expressions.info/cpp.html](https://www.regular-expressions.info/cpp.html)
- Tutoriel sur la manipulation de chaînes de caractères en C++ : [https://www.tutorialspoint.com/cplusplus/cpp_strings.htm](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)