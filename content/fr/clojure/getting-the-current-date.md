---
title:                "Obtenir la date actuelle"
html_title:           "Clojure: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Se préoccuper de la date actuelle peut sembler banal, mais cela peut être très utile dans de nombreuses situations en programmation. Cela peut être pour afficher la date sur une interface utilisateur, pour effectuer des opérations liées à la date, ou simplement pour suivre l'heure à laquelle une fonction ou un processus a été exécuté.

## Comment faire

Pour obtenir la date actuelle en Clojure, vous pouvez utiliser la fonction `java.util.Date` qui renvoie un objet représentant la date et l'heure actuelles.

```Clojure
(import java.util.Date)

(def current-date (Date.))
```

Vous pouvez ensuite utiliser la fonction `format` pour formater la date selon vos besoins.

```Clojure
(require '[clojure.java-time :as t])

(t/format current-date "dd-MM-yyyy") ;; 19-04-2021
```

Vous pouvez également utiliser la bibliothèque `java-time` pour une manipulation plus avancée de la date et de l'heure.

```Clojure
(require '[java-time :as jt])

(jt/year current-date) ;; 2021
(jt/month current-date) ;; :APRIL
(jt/day-of-month current-date) ;; 19
```

## Plongée en profondeur

La fonction `java.util.Date` renvoie un objet mutable, c'est-à-dire que sa valeur peut changer au fil du temps. Il est donc recommandé de l'assigner à une variable plutôt que de l'utiliser directement. De plus, cette fonction utilise le fuseau horaire de l'utilisateur, ce qui peut causer des incohérences dans certains cas.

Pour éviter ces problèmes, il est préférable d'utiliser la bibliothèque `java-time` qui renvoie un objet immuable et prend en compte le fuseau horaire par défaut.

## Voir aussi

- [Documentation Java - Classe Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Clojure.java-time](https://github.com/dm3/clojure.java-time)
- [Manipulation de dates et d'heures en Clojure](https://clojure.org/guides/java_time)