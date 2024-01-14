---
title:    "C++: Trouver la longueur d'une chaîne de caractères"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche courante dans le développement d'applications C++. Cela peut être utile pour la validation des entrées utilisateur, la manipulation de données et bien d'autres utilisations.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en C++, vous pouvez utiliser la fonction `strlen()` de la bibliothèque standard. Voici un exemple de code :

```C++
#include <iostream>
#include <cstring>

// Fonction qui prend une chaîne de caractères comme argument
// et renvoie sa longueur
int trouverLongueur(string chaine) {
  // Utilisez la fonction strlen() pour trouver la longueur de la chaîne
  int longueur = strlen(chaine);

  // Renvoie la longueur
  return longueur;
}

int main() {
  // Déclarez une chaîne de caractères
  string chaine = "Bonjour tout le monde !";

  // Appelez la fonction pour trouver la longueur de la chaîne
  int longueur = trouverLongueur(chaine);

  // Affichez la longueur de la chaîne
  cout << "La longueur de la chaîne est : " << longueur << endl;

  return 0;
}
```

Output :

    La longueur de la chaîne est : 22

## Plongez plus en profondeur

La fonction `strlen()` utilise une boucle pour parcourir chaque caractère de la chaîne et renvoie le nombre total de caractères. Il est important de noter que cette fonction ne compte que les caractères jusqu'au premier caractère nul (`\0`). Si la chaîne se termine par un caractère nul, celui-ci ne sera pas compté.

De plus, en utilisant la bibliothèque `<cstring>`, vous pouvez également utiliser la fonction `strnlen()`, qui prend un deuxième argument spécifiant le nombre maximum de caractères à parcourir. Cela peut être utile pour éviter les dépassements de mémoire et les erreurs de segmentation.

## Voir aussi

- [Documentation sur `strlen()`](https://www.cplusplus.com/reference/cstring/strlen/)
- [Documentation sur `strnlen()`](https://www.cplusplus.com/reference/cstring/strnlen/)