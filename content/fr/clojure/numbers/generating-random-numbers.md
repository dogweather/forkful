---
title:                "Génération de nombres aléatoires"
aliases:
- /fr/clojure/generating-random-numbers.md
date:                  2024-01-27T20:33:12.760185-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Générer des nombres aléatoires en programmation consiste à créer des valeurs qui ne peuvent pas être prédites logiquement à l'avance. Les programmeurs font cela pour diverses raisons, y compris générer des identifiants uniques, simuler des scénarios dans le développement de jeux, ou sélectionner des échantillons aléatoires à partir de données pour analyse.

## Comment faire :

En Clojure, la génération de nombres aléatoires est simple, et il existe quelques fonctions intégrées qui peuvent être utilisées immédiatement.

Pour générer un nombre à virgule flottante aléatoire entre 0 (inclus) et 1 (exclus), vous pouvez utiliser la fonction `rand` :

```Clojure
(rand)
;; Exemple de sortie : 0.7094245047062917
```

Si vous avez besoin d'un entier dans une plage spécifique, utilisez `rand-int` :

```Clojure
(rand-int 10)
;; Exemple de sortie : 7
```

Cela vous donne un entier aléatoire entre 0 (inclus) et le nombre que vous passez en argument (exclus).

Pour générer un nombre aléatoire dans une plage spécifique (non limitée aux entiers), vous pouvez combiner `rand` avec de l'arithmétique :

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Utilisation
(rand-range 10 20)
;; Exemple de sortie : 14.857457734992847
```

Cette fonction `rand-range` retournera un nombre à virgule flottante aléatoire entre les valeurs `min` et `max` que vous spécifiez.

Pour des scénarios nécessitant des distributions plus complexes ou des séquences de nombres aléatoires où la répétabilité est nécessaire (utilisation de graines), vous pourriez avoir besoin de regarder dans des bibliothèques supplémentaires qui vont au-delà de ce qui est intégré.

## Plongée Profonde

Le mécanisme sous-jacent pour générer des nombres aléatoires dans la plupart des langages de programmation, y compris Clojure, repose généralement sur un générateur de nombres pseudo-aléatoires (PRNG). Un PRNG utilise un algorithme pour produire une séquence de nombres qui approximent les propriétés des nombres aléatoires. Il est important de noter que, parce qu'ils sont générés algorithmiquement, ils ne sont pas véritablement aléatoires mais peuvent être suffisants pour la plupart des utilisations pratiques.

Aux premiers jours de l'informatique, générer des nombres aléatoires de haute qualité était un défi significatif, conduisant au développement de divers algorithmes pour améliorer l'aléa et la distribution. Pour Clojure, les fonctions intégrées, telles que `rand` et `rand-int`, sont pratiques pour une utilisation quotidienne et couvrent un large éventail de cas d'utilisation courants.

Cependant, pour les applications nécessitant une sécurité cryptographique ou des méthodes d'échantillonnage statistique plus complexes, les développeurs Clojure se tournent souvent vers des bibliothèques externes qui offrent des PRNGs plus robustes et spécialisés. Des bibliothèques telles que `clj-random` fournissent un accès à une plus grande variété d'algorithmes et un meilleur contrôle du seeding, ce qui peut être crucial pour les simulations, les applications cryptographiques, ou tout domaine où la qualité et la prédictibilité de la séquence de nombres aléatoires pourraient avoir des implications significatives.

Alors que les capacités intégrées de Clojure pour générer des nombres aléatoires sont adéquates pour de nombreuses tâches, explorer des bibliothèques externes peut offrir des insights plus approfondis et des options pour des applications sur mesure ou plus critiques.
