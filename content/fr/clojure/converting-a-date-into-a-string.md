---
title:    "Clojure: Convertir une date en chaîne de caractères"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Pourquoi

La conversion d'une date en chaîne de caractères est une tâche courante dans la programmation Clojure. Cela peut être utile pour afficher une date dans un format spécifique ou pour la comparer à d'autres dates sous forme de chaîne de caractères. Dans cet article, nous allons explorer comment effectuer cette conversion de manière efficace et sans soucis.

## Comment Faire

Nous allons utiliser la fonction `str` pour convertir notre date en chaîne de caractères. Tout d'abord, définissons une date avec la fonction `local-date` de la bibliothèque `java-time` :

```Clojure
(require '[java-time :as t])

(def date (t/local-date 2022 1 1))
```

Ensuite, nous utiliserons `str` pour convertir cette date en une chaîne de caractères au format "AAAA-MM-JJ" :

```Clojure
(str date)
;; output: "2022-01-01"
```

Si nous souhaitons un format différent, nous pouvons utiliser une combinaison de `str` et des fonctions `t/day`, `t/month` et `t/year` pour extraire chaque partie de la date et les concaténer dans l'ordre souhaité :

```Clojure
(str (t/day date) "/" (t/month date) "/" (t/year date))
;; output: "01/01/2022"
```

## Plongée Profonde

La fonction `str` convertit automatiquement une date en utilisant le format "AAAA-MM-JJ". Cependant, il est également possible d'utiliser la fonction `t/format` pour spécifier un format personnalisé. Cette fonction prend en paramètre un format de chaîne de caractères et la date à convertir. Voici un exemple :

```Clojure
(t/format "Le %d %B %Y" date)
;; output: "Le 1 janvier 2022"
```

Il est important de noter que les spécificateurs de format pour la fonction `t/format` sont différents de ceux utilisés avec `str`. Par exemple, "%d" est utilisé pour représenter le jour de la date tandis que "%B" représente le nom complet du mois.

# Voir Aussi

- La documentation officielle de la bibliothèque `java-time` : https://github.com/java-time/java-time
- Une liste complète des spécificateurs de format pour la fonction `t/format` : https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/format/DateTimeFormatter.html#patterns