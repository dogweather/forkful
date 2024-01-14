---
title:    "Clojure: Calcul d'une date dans le futur ou le passé."
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles on pourrait vouloir savoir comment calculer une date dans le futur ou le passé en Clojure. Cela peut être utile pour planifier des événements, gérer des tâches ou suivre des échéances. Peut-être que vous souhaitez créer un programme qui génère automatiquement des rappels pour les anniversaires ou les dates d'échéance importantes. Quelle que soit la raison, savoir comment calculer une date dans le futur ou le passé en Clojure peut être un outil pratique.

## Comment faire

La première étape pour calculer une date dans le futur ou le passé en Clojure est d'importer la bibliothèque `java.time.LocalDate`. Cette bibliothèque fournit des fonctions utiles pour créer et manipuler des dates.

```
(import 'java.time.LocalDate)
```

Ensuite, utilisez la fonction `LocalDate/of` pour créer une nouvelle date en spécifiant l'année, le mois et le jour.

```
(def date (LocalDate/of 2020 12 31))
```

Pour calculer une date dans le futur, vous pouvez utiliser la fonction `plusDays`. Par exemple, pour ajouter 10 jours à la date que vous avez créée, utilisez la fonction comme ceci :

```
(def future-date (.plusDays date 10))
```

De même, pour calculer une date dans le passé, utilisez la fonction `minusDays`.

```
(def past-date (.minusDays date 10))
```

Pour afficher la date sous un format plus lisible, vous pouvez utiliser la fonction `format` en spécifiant un modèle. Par exemple, pour obtenir une date sous le format "DD-MM-YYYY", utilisez cette fonction :

```
(def date-format (.format date (java.time.format.DateTimeFormatter.ofPattern "dd-MM-yyyy")))
```

## Plongée en profondeur

En plus des fonctions mentionnées ci-dessus, la bibliothèque `java.time.LocalDate` offre une variété d'autres fonctions pour manipuler et formater des dates en Clojure. La documentation officielle de la bibliothèque fournit une liste complète de ces fonctions et leurs utilisations.

Il est également possible de créer des objets `java.time.LocalDateTime` et `java.time.LocalTime` pour représenter des dates et heures précises, ainsi que de les combiner avec des opérations arithmétiques pour effectuer des calculs plus complexes.

## Voir aussi
- La documentation complète de la bibliothèque `java.time` : https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html
- Un tutoriel sur les fonctions temporelles en Clojure : https://www.braveclojure.com/function-practice/#Time_Manipulation_in_Dates