---
title:                "Clojure: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez sur des projets où les dates sont importantes, il peut être utile de pouvoir calculer une date dans le futur ou dans le passé. Cela peut vous aider à planifier des événements, à gérer des retards ou à prédire des échéances. En utilisant Clojure, vous pouvez facilement effectuer ces calculs pour rendre votre travail plus pratique et plus efficace.

# Comment faire

Pour calculer une date dans le futur ou dans le passé en utilisant Clojure, vous pouvez suivre ces étapes simples :

1. Tout d'abord, importez la librairie `clojure.java-time` dans votre projet en utilisant la directive `require`.
```
(require '[clojure.java-time :as time])
```
2. Ensuite, déterminez une date de référence en utilisant la fonction `interval`.
```
(def date-ref (LocalDate/of 2021 6 15))
```
3. Maintenant, vous pouvez utiliser les fonctions `plus` ou `minus` pour ajouter ou soustraire une certaine quantité de temps à votre date de référence.
```
(time/plus date-ref (period/+ 7 :days))
```
Cela renverra une date, 7 jours après la date de référence.

```
(time/minus date-ref (period/+ 1 :month))
```
Cela renverra une date, 1 mois avant la date de référence.

# Profondeur technique

Clojure utilise la bibliothèque Java `java.time` pour gérer les dates et les heures. Cela signifie que vous avez accès à toutes les fonctions et les classes de cette bibliothèque en utilisant `clojure.java-time`. Vous pouvez également utiliser d'autres unités de temps telles que les années, les heures, les minutes, etc.

De plus, Clojure est un langage fonctionnel, ce qui signifie que vous pouvez facilement composer des fonctions pour effectuer des calculs plus complexes sur les dates. Par exemple, vous pouvez calculer une date dans le futur en ajoutant une quantité de temps en fonction de certains paramètres dynamiques dans votre code.

# Voir aussi

- [Documentation officielle de clojure.java-time](https://clojure.github.io/java-time/)
- [Guide de référence rapide pour manipuler les dates en Clojure](https://lispcast.com/how-to-deal-with-time-in-clojure/)