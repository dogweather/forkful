---
title:    "Clojure: Calculer une date dans le futur ou le passé"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi 

Calculer une date dans le futur ou dans le passé peut être utile dans de nombreuses situations en programmation, que ce soit pour planifier des tâches, pour des échéances ou pour des analyses temporelles. Heureusement, Clojure offre des fonctionnalités simples pour effectuer ces calculs de manière efficace.

## Comment faire 

Pour calculer une date dans le futur ou dans le passé en Clojure, nous pouvons utiliser la fonction `+` et la fonction `+`, qui prennent toutes deux un paramètre de type temps et un paramètre de type durée en années, mois, jours, heures, minutes et secondes. Nous pouvons également utiliser la bibliothèque `clj-time` qui offre des fonctions plus avancées pour travailler avec des dates dans Clojure.

Voici un exemple de code qui calcule une date dans le futur en utilisant la fonction `+` :

```Clojure
; Importer la bibliothèque clj-time
(require '[clj-time.core :as t])

(def maintenant (t/now))

; Ajouter 2 jours à la date actuelle
(def dateFutur (t/+ maintenant (t/days 2)))

; Afficher la date dans le format souhaité
(t/formatter "dd-MM-yyyy" dateFutur)
; Output: 08-08-2021
```

Et voici un exemple de code qui calcule une date dans le passé en utilisant la fonction `-` :

```Clojure
; Importer la bibliothèque clj-time
(require '[clj-time.core :as t])

(def maintenant (t/now))

; Soustraire 1 mois à la date actuelle
(def datePassee (t/- maintenant (t/months 1)))

; Afficher la date dans le format souhaité
(t/formatter "dd-MM-yyyy" datePassee)
; Output: 07-07-2021
```

## Profonde plongée 

Lorsque nous utilisons des fonctions de calcul de dates en Clojure, il est important de comprendre que les valeurs de temps sont représentées en tant qu'objets de fraction fixe et les valeurs de durée en tant qu'objets de fraction. Cela signifie que les opérations de date et de durée se font avec une précision au millième de seconde.

Il est également important de noter que les fonctions `+` et `-` acceptent des paramètres de durée négatifs pour effectuer des calculs dans le passé.

En utilisant la bibliothèque `clj-time`, nous pouvons également effectuer d'autres opérations telles que la comparaison de dates, le décalage de dates et la conversion entre les différents types de temps (UTC, heure locale, etc.).

## Voir aussi 

- La documentation officielle de Clojure sur la manipulation de dates : https://clojuredocs.org/clojure.java-time/introduction
- La documentation officielle de la bibliothèque clj-time : https://clj-time.github.io/clj-time/
- Un tutoriel complet sur la gestion du temps en Clojure : https://tech.marksblogg.com/clojure-datetime-library.html