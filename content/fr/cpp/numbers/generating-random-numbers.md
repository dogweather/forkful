---
title:                "Génération de nombres aléatoires"
aliases:
- /fr/cpp/generating-random-numbers.md
date:                  2024-01-27T20:32:47.162565-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La génération de nombres aléatoires en programmation consiste à créer des séquences de nombres qui ne présentent aucun ordre ou motif prévisible. Les programmeurs utilisent souvent ces nombres à diverses fins telles que la simulation d'événements imprévisibles, dans les tests et le débogage, et dans les algorithmes de jeux pour garantir l'équité ou l'imprévisibilité.

## Comment faire :

Pour générer des nombres aléatoires en C++, vous utiliseriez typiquement l'en-tête `<random>`, qui a été introduit dans C++11, offrant une large gamme de facilités pour générer des nombres aléatoires à partir de diverses distributions.

```C++
#include <iostream>
#include <random>

int main() {
    // Initialiser un moteur aléatoire
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Définir l'intervalle [0, 99] inclusivement
    std::uniform_int_distribution<> distrib(0, 99); 

    // Générer et imprimer 5 nombres aléatoires dans l'intervalle défini
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Cet exemple de code initialise un générateur de nombres aléatoires Mersenne Twister avec une graine provenant de `std::random_device`. Il définit ensuite une distribution entière uniforme dans la plage [0, 99] et imprime finalement 5 nombres aléatoires de cette distribution.

Le résultat pourrait ressembler à cela, mais gardez à l'esprit que chaque exécution produira probablement des résultats différents :

```
45 67 32 23 88
```

## Plongée profonde :

Historiquement, la génération de nombres aléatoires en C++ reposait fortement sur la fonction `rand()` et la fonction de semence `srand()`, trouvées dans l'en-tête `<cstdlib>`. Cependant, cette approche a souvent été critiquée pour son manque d'uniformité et de prévisibilité dans la distribution des nombres générés.

L'introduction de l'en-tête `<random>` dans C++11 a marqué une amélioration significative, offrant un système sophistiqué pour produire des nombres aléatoires. Les installations fournies comprennent une variété de moteurs (comme `std::mt19937` pour Mersenne Twister) et de distributions (comme `std::uniform_int_distribution` pour la distribution uniforme des entiers) qui peuvent être combinés pour répondre aux besoins spécifiques du programmeur, conduisant à un comportement plus prévisible, de meilleures performances et une plus grande flexibilité.

Bien que la bibliothèque `<random>` soit bien meilleure que l'ancienne approche `rand()`, il convient de noter que la génération de nombres véritablement aléatoires - en particulier à des fins cryptographiques - repose toujours sur des considérations supplémentaires. Pour les applications cryptographiques, des bibliothèques conçues spécifiquement pour la sécurité, qui utilisent souvent des sources d'entropie matérielles, doivent être utilisées à la place.
