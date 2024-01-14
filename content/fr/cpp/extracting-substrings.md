---
title:                "C++: Extraction de sous-chaînes"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Les sous-chaînes, aussi appelées "substrings", sont des parties d'une chaîne de caractères plus grande. Les extraire peut être extrêmement utile lorsque vous travaillez avec des données textuelles dans votre programme C++. Cela permet de manipuler et de traiter des portions spécifiques de texte de manière plus efficace.

## Comment faire

Pour extraire une sous-chaîne dans un programme C++, vous pouvez utiliser la fonction `substr` de la classe `string`. Voici un exemple de code qui extrait une sous-chaîne à partir d'une chaîne de caractères :

```C++
#include <iostream>
using namespace std;

int main() {
    string texte = "Bonjour tout le monde";
    string sous_chaine = texte.substr(8,4); // à partir de l'index 8, 4 caractères sont extraits
    cout << sous_chaine << endl; // affiche "tout"
    return 0;
}
```

Dans cet exemple, la fonction `substr` prend deux arguments : l'index de départ et le nombre de caractères à extraire. Le résultat est stocké dans une nouvelle chaîne de caractères `sous_chaine`.

Vous pouvez également spécifier un seul argument à la fonction `substr` pour extraire tous les caractères à partir d'un index donné jusqu'à la fin de la chaîne.

```C++
string sous_chaine = texte.substr(3); // à partir de l'index 3 jusqu'à la fin
cout << sous_chaine << endl; // affiche "jour tout le monde"
```

Il est également possible de fournir un index négatif pour extraire une sous-chaîne à partir de la fin.

```C++
string sous_chaine = texte.substr(-6, 3); // à partir des 6 derniers caractères, 3 sont extraits
cout << sous_chaine << endl; // affiche "mon"
```

## Plongée en profondeur

La fonction `substr` n'est pas la seule méthode pour extraire des sous-chaînes en C++. Vous pouvez également utiliser des boucles et des fonctions de manipulation de chaînes de caractères telles que `find` et `replace`.

Vous pouvez également utiliser des expressions régulières pour extraire des sous-chaînes qui correspondent à un modèle spécifique. Cela peut être particulièrement utile pour extraire des informations telles que des dates, des adresses ou des numéros de téléphone dans du texte.

## Voir aussi

- [Documentation officielle de la fonction substr](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutoriel sur la manipulation de chaînes de caractères en C++](https://www.tutorialspoint.com/cpp_standard_library/string.htm)
- [Guide des expressions régulières en C++](https://www.regular-expressions.info/cpp.html)