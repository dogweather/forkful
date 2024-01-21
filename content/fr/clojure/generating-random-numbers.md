---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:43.326603-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? 
Générer des nombres aléatoires, c'est comme lancer un dé virtuel. Les programmeurs le font pour ajouter de l'imprévisibilité, tester des scénarios, ou encore simuler des processus dans leurs programmes.

## How to:
Générer un numéro simple :

```clojure
(rand) ; Un nombre aléatoire entre 0 et 1.
```

Pour un entier :

```clojure
(rand-int 10) ; Un entier aléatoire entre 0 et 9.
```

Si vous voulez une série de nombres :

```clojure
(repeatedly 5 #(rand-int 10)) ; Cinq nombres aléatoires entre 0 et 9.
```

Un peu plus ciblé, disons entre 50 et 100 :

```clojure
(map #(+ 50 %) (repeatedly 5 #(rand-int 51))) ; Cinq nombres aléatoires entre 50 et 100.
```

## Deep Dive
Clojure, en se reposant sur la JVM, a accès aux fonctionnalités de Java pour générer des nombres aléatoires. Par le passé, les gens utilisaient des méthodes moins sophistiquées, comme des tables de nombres aléatoires. Mais aujourd'hui, nous avons des algorithmes complexes comme Mersenne Twister. En Clojure, `rand` et `rand-int` font le travail pour nous, mais sachez qu'ils se basent sur `java.util.Random` sous le capot. Pour les besoins critiques en sécurité, on préfère `java.security.SecureRandom`, mais vous n'avez pas besoin de vous en soucier pour l'usage général.

## See Also
Pour plus de détails, consultez la documentation officielle :

- Documentation Clojure pour `rand` et `rand-int`: [https://clojuredocs.org/clojure.core/rand](https://clojuredocs.org/clojure.core/rand) 
- Une discussion sur l'utilisation de nombres aléatoires en Clojure sur StackOverflow: [https://stackoverflow.com/questions/tagged/clojure+random](https://stackoverflow.com/questions/tagged/clojure+random)
- Pour approfondir l'algorithme de Mersenne Twister : [https://en.wikipedia.org/wiki/Mersenne_Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)