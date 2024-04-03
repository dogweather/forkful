---
date: 2024-01-26 03:44:38.241383-07:00
description: "Arrondir les nombres consiste \xE0 ajuster un nombre au plus proche\
  \ entier ou \xE0 une certaine pr\xE9cision d\xE9cimale. Nous arrondissons les nombres\
  \ pour les\u2026"
lastmod: '2024-03-13T22:44:57.276780-06:00'
model: gpt-4-0125-preview
summary: "Arrondir les nombres consiste \xE0 ajuster un nombre au plus proche entier\
  \ ou \xE0 une certaine pr\xE9cision d\xE9cimale."
title: Arrondir les nombres
weight: 13
---

## Comment faire :
En Clojure, nous utilisons principalement `Math/round`, `Math/floor`, et `Math/ceil` :

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Pour des décimales spécifiques, nous multiplions, arrondissons et divisons :

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Plongée Profonde
Avant les langages de programmation modernes, l'arrondissement était un processus manuel, pensez à l'abaque ou au papier. En programmation, il est crucial pour la représentation des nombres en raison des limitations de précision des nombres à virgule flottante.

Les alternatives pour l'arrondissement incluent l'utilisation de la classe `BigDecimal` pour le contrôle de la précision ou des bibliothèques comme `clojure.math.numeric-tower` pour des fonctions mathématiques avancées. Le `Math/round` de Clojure s'appuie sur les fonctions de Java `Math.round`, `Math/floor`, et `Math/ceil`, ce qui signifie qu'il hérite des mêmes nuances de float et de double.

D'un point de vue d'implémentation, lors de l'arrondissement en Clojure, rappelez-vous qu'il utilise automatiquement une double précision lorsqu'il traite des décimales. Attention aux erreurs d'arrondi !

## Voir Aussi
- API Math de Clojure : [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- API Math de Java : [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Comprendre la Précision des Nombres à Virgule Flottante : [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
