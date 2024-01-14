---
title:                "C++: Création de nombres aléatoires"
simple_title:         "Création de nombres aléatoires"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Générer des nombres aléatoires peut sembler être un sujet banal en programmation, mais c'est en fait une compétence très utile à maîtriser. En utilisant des nombres aléatoires, vous pouvez créer des simulations, des jeux et même des tests de logiciels plus réalistes.

## Comment faire

La génération de nombres aléatoires en C++ peut sembler intimidante pour les débutants, mais ne vous inquiétez pas, c'est en fait assez simple! Tout d'abord, vous devez inclure la bibliothèque <random> dans votre code pour pouvoir utiliser les fonctions de génération aléatoire.

```C++
#include <iostream>
#include <random>
using namespace std;

int main() {

    // Déclaration de la fonction de génération aléatoire
    random_device rd;
    // Choix de la plage de nombres aléatoires
    uniform_int_distribution<int> dist(1, 10);
    // Génération d'un nombre aléatoire
    int random_number = dist(rd);
    
    // Affichage du résultat
    cout << "Le nombre aléatoire est : " << random_number << endl;
    
    return 0;
}
```
Output:
Le nombre aléatoire est : 8

Dans cet exemple, nous avons utilisé la fonction random_device pour générer une graine aléatoire et la fonction de distribution uniforme pour choisir une plage de nombres aléatoires, puis nous avons généré un nombre aléatoire en utilisant la graine et la distribution. Vous pouvez également utiliser d'autres types de distributions en fonction de vos besoins.

## Deep Dive

La génération de nombres aléatoires en C++ est basée sur des algorithmes de génération pseudo-aléatoires. Cela signifie que les nombres générés ne sont pas vraiment aléatoires, mais ils sont suffisamment imprévisibles pour être considérés comme des nombres aléatoires pour la plupart des applications.

La fonction random_device que nous avons utilisée dans notre exemple est en fait un générateur de nombres aléatoires semi-aléatoires basé sur le bruit du système. Cela signifie que le résultat sera différent à chaque fois que vous exécutez le programme.

Si vous avez besoin de résultats réellement aléatoires, vous pouvez utiliser des générateurs de nombres aléatoires véritablement aléatoires basés sur des phénomènes physiques tels que le bruit radioactif ou les températures atmosphériques.

## Voir aussi

- [Documentation officielle de la bibliothèque <random> en C++](https://en.cppreference.com/w/cpp/numeric/random)
- [Génération de nombres aléatoires en C++ - Tutoriel vidéo](https://www.youtube.com/watch?v=ubOJ5n2PnP0)
- [Générateur de nombres aléatoires basé sur le temps en C++](https://www.techiedelight.com/generate-random-numbers-cpp/)