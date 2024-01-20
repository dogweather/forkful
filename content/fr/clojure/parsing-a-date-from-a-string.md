---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi ?

Parser une date à partir d'une chaîne de caractères consiste à extraire et interpréter les informations de date et d'heure d'un texte formaté. Les programmeurs le font pour pouvoir manipuler et utiliser ces informations dans leur code.

## Comment faire :

En Clojure, nous utilisons généralement la bibliothèque `clj-time` pour parser des dates. Voici un exemple de la façon de le faire :

```Clojure
(require '[clj-time.format :as f])
(def formatter (f/formatter "dd/MM/yyyy"))
(def date (f/parse formatter "31/12/2021"))
```

Lorsque vous exécutez cette code, `date` sera une instance de la classe `org.joda.time.DateTime` qui représente le 31 décembre 2021.

## Exploration en profondeur :

Historiquement, `clj-time` est une très ancienne bibliothèque de dates en Clojure. Elle est construite sur `Joda-Time`, qui était la principale bibliothèque de dates et heures pour Java avant l'introduction de l'API `java.time` dans Java 8.

En ce qui concerne les alternatives, `java.time` a de facto remplacé `Joda-Time` en Java et de nombreuses nouvelles bibliothèques Clojure, comme `clojure.java-time`, sont construites sur `java.time`. En fonction de votre situation, vous pourriez préférer utiliser une de ces autres bibliothèques.

En ce qui concerne les détails de l'implémentation, il est important de noter que le parsing de chaînes de date peut échouer. Par exemple, si le format de la chaîne ne correspond pas au formatteur spécifié, une exception sera levée. Il est donc judicieux de gérer ces situations avec un bloc `try/catch`.

## Voir aussi :

Pour plus d'informations sur la manipulation des dates en Clojure, consultez les liens suivants :

- La documentation de clj-time : [ici](https://github.com/clj-time/clj-time)
- La documentation de `java.time` pour plus d'informations sur l'évolution moderne du traitement de la date et l'heure : [ici](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)