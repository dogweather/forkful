---
date: 2024-01-26 04:38:39.398329-07:00
description: "Les nombres complexes \xE9tendent les nombres r\xE9els avec une partie\
  \ suppl\xE9mentaire, l'unit\xE9 imaginaire 'i'. Les programmeurs les utilisent dans\
  \ divers\u2026"
lastmod: '2024-02-25T18:49:54.163488-07:00'
model: gpt-4-0125-preview
summary: "Les nombres complexes \xE9tendent les nombres r\xE9els avec une partie suppl\xE9\
  mentaire, l'unit\xE9 imaginaire 'i'. Les programmeurs les utilisent dans divers\u2026"
title: Manipulation des nombres complexes
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Les nombres complexes étendent les nombres réels avec une partie supplémentaire, l'unité imaginaire 'i'. Les programmeurs les utilisent dans divers domaines, notamment le traitement du signal, la théorie électromagnétique et les fractales, où les calculs impliquant la racine carrée d'un nombre négatif sont courants.

## Comment faire :
Clojure offre un support intégré pour les nombres complexes via la classe utilitaire `clojure.lang.Numbers`. Utilisez `complex` pour créer des nombres complexes et effectuer des calculs arithmétiques.

```clojure
;; Création de nombres complexes
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; Addition
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; Soustraction
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; Multiplication
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; Division
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3,5 + 3,5i"]

;; Conjugué
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## Approfondissement
Les nombres complexes ont été formalisés par des mathématiciens comme Gauss et Euler au 18ème siècle. Bien qu'initialement accueillis avec scepticisme, ils sont depuis devenus cruciaux dans la science et l'ingénierie modernes. Clojure n'a pas de type de nombre complexe natif comme certains langages (par exemple, Python), mais l'interopérabilité Java incluse peut gérer les opérations nécessaires via la classe `clojure.lang.Numbers`.

Le `java.lang.Complex` de Java est une alternative robuste, offrant plus de fonctionnalités et des optimisations potentielles. L'interopérabilité sous-jacente de Clojure rend le travail avec les bibliothèques Java facile.

Sous le capot, l'arithmétique des nombres complexes implique l'ajout et la multiplication des parties réelles et imaginaires, avec la règle clé que `i^2 = -1`. La division des nombres complexes peut être plus compliquée, nécessitant généralement le conjugué pour éviter la division par des nombres complexes.

## Voir également
- The ClojureDocs, pour une référence rapide : https://clojuredocs.org/
- L'API Java pour `java.lang.Complex` : https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- La page Wikipédia sur les nombres complexes pour les curieux de mathématiques : https://en.wikipedia.org/wiki/Complex_number
