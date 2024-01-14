---
title:                "C++: Suppression de caractères correspondant à un modèle"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un modèle peut être utile dans de nombreux cas en programmation C++. Par exemple, cela peut être nécessaire lors du traitement de données utilisateur ou pour nettoyer des chaînes de caractères avant de les utiliser dans des opérations plus complexes.

## Comment faire

Pour supprimer des caractères correspondant à un modèle, il faut utiliser la fonction `erase` de la classe `std::string`. Cette fonction prend en paramètres l'index de départ et le nombre de caractères à supprimer. Nous pouvons combiner cette fonction avec `find` pour trouver l'index à partir duquel supprimer les caractères correspondants à notre modèle. Voici un exemple de code :

```C++
#include <iostream>
#include <string>

int main() {
    std::string texte = "Bonjour tout le monde !";
    std::string modele = "tou";

    int index = texte.find(modele); // Trouve l'index à partir duquel supprimer les caractères
    texte.erase(index, modele.size()); // Supprime les caractères correspondant au modèle
    std::cout << texte << std::endl;

    return 0;
}
```

L'output sera :

```
Bonjour le monde !
```

## Plongée en profondeur

Il est important de noter que la fonction `erase` modifie directement la chaîne de caractères sur laquelle elle est appliquée. Cela signifie que la chaîne d'origine sera modifiée après l'appel à la fonction `erase`. Il est également possible de combiner `erase` avec d'autres fonctions de la classe `std::string` pour effectuer des actions plus complexes. Par exemple, nous pouvons utiliser `substr` pour supprimer une partie spécifique d'une chaîne de caractères en utilisant un modèle.

## Voyez aussi

- [Documentation sur la fonction `erase`](https://www.cplusplus.com/reference/string/string/erase/)
- [Documentation sur la fonction `substr`](https://www.cplusplus.com/reference/string/string/substr/)