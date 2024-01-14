---
title:    "Clojure: Suppression des caractères correspondant à un modèle"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

La suppression de caractères correspondant à un modèle peut être utile dans de nombreuses situations, notamment pour nettoyer des données ou pour effectuer des transformations dans des chaînes de caractères.

## Comment faire

Il existe plusieurs façons de supprimer des caractères correspondant à un modèle en utilisant Clojure. Voici deux exemples de code qui illustrent différentes approches :

```Clojure
;; Supprimer tous les caractères non numériques d'une chaîne de caractères

(def chaine "123abc456")
(def nouveau-chaine (clojure.string/replace chaine #"\D" ""))
(print nouveau-chaine)

```

Le résultat de ce code serait "123456", car tous les caractères non numériques ont été supprimés de la chaîne d'origine.

```Clojure
;; Supprimer tous les espaces d'une chaîne de caractères

(def chaine "Bonjour, je suis un exemple")
(def nouveau-chaine (clojure.string/replace chaine #"\s" ""))
(print nouveau-chaine)

```

Le résultat de ce code serait "Bonjour,jesuisunexemple", car tous les espaces ont été supprimés de la chaîne d'origine.

## Plongée profonde

En utilisant la fonction "replace" de la bibliothèque de chaînes de caractères de Clojure, il est possible de supprimer des caractères correspondant à un modèle spécifique. Le modèle peut être défini à l'aide d'expressions régulières, ce qui offre une grande flexibilité dans la sélection des caractères à supprimer.

De plus, il est également possible d'utiliser d'autres fonctions de la bibliothèque de chaînes de caractères, telles que "replace-first" ou "replace-nth", pour cibler des occurrences spécifiques du modèle à supprimer.

## Voir aussi

- Documentation officielle de la bibliothèque de chaînes de caractères Clojure : https://clojuredocs.org/clojure.string/replace
- Tutoriel sur les expressions régulières en Clojure : https://learnxinyminutes.com/docs/fr-fr/clojure-fr/