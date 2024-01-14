---
title:    "C++: Extraction de sous-chaînes"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur C++ en herbe, vous avez probablement déjà entendu parler de l'extraction de sous-chaînes. Mais peut-être que vous ne savez pas exactement pourquoi c'est utile ou comment le faire correctement. Dans cet article, nous allons plonger dans le monde des sous-chaînes et vous expliquer pourquoi vous devriez les utiliser.

## Comment faire

L'extraction de sous-chaînes consiste à extraire une partie spécifique d'une chaîne de caractères. Cela peut être utile pour diverses tâches telles que la recherche de mots-clés dans une chaîne, ou pour obtenir une partie spécifique d'une URL.

Voici un exemple de code en C++ pour extraire une sous-chaîne d'une chaîne :

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string s = "Bonjour tout le monde !";
    string sousChaine = s.substr(8, 11); //extrait la sous-chaîne commençant à l'indice 8 et ayant une longueur de 11 caractères
    cout << sousChaine; //affiche "tout le monde"
    return 0;
}
```

Dans cet exemple, nous utilisons la fonction `substr()` de la classe `string` pour extraire la sous-chaîne. Elle prend deux paramètres : l'indice de départ et la longueur de la sous-chaîne désirée.

Voici un autre exemple pour extraire une partie spécifique d'une URL :

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string url = "https://www.example.com/article/123456";
    string id = url.substr(30); //extrait l'ID de l'article en commençant à l'indice 30 jusqu'à la fin de la chaîne
    cout << id; //affiche "123456"
    return 0;
}
```

## Plongée en profondeur

Maintenant que vous savez comment extraire des sous-chaînes en C++, vous pouvez également vouloir en savoir plus sur le fonctionnement interne de cette opération. Une sous-chaîne est simplement une vue de la chaîne d'origine, elle ne fait donc pas de copie de la mémoire. Cela signifie que si vous modifiez la sous-chaîne, la chaîne d'origine sera également modifiée.

Il est également important de noter que les indices dans une chaîne commencent à 0, donc si vous voulez extraire une sous-chaîne de l'indice 0 jusqu'à un indice donné, il vous suffit simplement de spécifier la longueur de la sous-chaîne.

## Voir aussi

Maintenant que vous savez comment extraire des sous-chaînes en C++, vous pouvez peut-être être intéressé par d'autres sujets liés à la manipulation de chaînes de caractères. Voici quelques liens qui pourraient vous être utiles :

- [Comment concaténer des chaînes de caractères en C++](https://www.example.com/concatenation-cpp)
- [Comment convertir une chaîne de caractères en entier en C++](https://www.example.com/conversion-entier-cpp)
- [Comment trouver une chaîne dans une autre chaîne en C++](https://www.example.com/recherche-chaine-cpp)

Maintenant, à vous de jouer ! Utilisez l'extraction de sous-chaînes dans vos projets pour améliorer votre code et faciliter vos tâches de manipulation de chaînes en C++.