---
date: 2024-01-20 17:30:43.570334-07:00
description: "Calculer une date dans le futur ou le pass\xE9, c'est trouver une date\
  \ \xE0 partir d'une autre, avant ou apr\xE8s un certain temps. Les programmeurs\
  \ font \xE7a pour\u2026"
lastmod: '2024-03-13T22:44:57.296295-06:00'
model: gpt-4-1106-preview
summary: "Calculer une date dans le futur ou le pass\xE9, c'est trouver une date \xE0\
  \ partir d'une autre, avant ou apr\xE8s un certain temps. Les programmeurs font\
  \ \xE7a pour\u2026"
title: "Calcul d'une date future ou pass\xE9e"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Calculer une date dans le futur ou le passé, c'est trouver une date à partir d'une autre, avant ou après un certain temps. Les programmeurs font ça pour gérer des échéances, planifier des événements, ou suivre le temps qui passe.

## Comment faire :

```clojure
;; Ajoutons 3 jours à aujourd'hui
(require '[java-time :as jt])

(def today (jt/local-date))
(def in-three-days (jt/plus-days today 3))

(println "Aujourd'hui, c'est :" today)
(println "Dans trois jours, ce sera :" in-three-days)
```

```clojure
;; Soustrayons 2 semaines de la date courante
(require '[java-time :as jt])

(def two-weeks-ago (jt/minus-weeks (jt/local-date) 2))

(println "Il y a deux semaines, c'était :" two-weeks-ago)
```

## Plongée en profondeur

Calculer une date dans le futur ou le passé n'est pas un concept nouveau. L'homme a toujours eu besoin de mesurer et de prévoir le temps. En informatique, cette fonctionnalité est essentielle pour tout, de la simple planification d'un rappel à la programmation de transactions financières. 

Historiquement, les programmeurs utilisaient des structures et des fonctions basiques de manipulation de temps, souvent avec des résultats imprévisibles à cause des complexités des calendriers et des fuseaux horaires. Ainsi, des bibliothèques robustes telles que `java.time` que Clojure utilise à présent, ont été développées.

Alternativement, Clojure permet aussi l'utilisation de bibliothèques externes telles que 'clj-time', une écharpe autour de Joda-Time, mais 'java.time' est devenu la solution privilégiée car elle est plus moderne et intégrée à Java 8 et versions ultérieures.

Concernant l'implémentation, il est impératif de gérer correctement les fuseaux horaires et les particularités du calendrier (comme les années bissextiles) pour éviter des erreurs. La bibliothèque `java-time` traite ces subtilités pour vous.

## Voir aussi

- Documentation de 'java.time': https://clojure.github.io/clojure/changes/javadoc/10.1.822/java-time.html
- Bibliothèque 'clj-time' sur GitHub: https://github.com/clj-time/clj-time
- Guide Clojure sur les dates et heures: https://clojure.org/guides/deps_and_cli
- Article sur la manipulation du temps dans divers langages de programmation: https://en.wikipedia.org/wiki/System_time
