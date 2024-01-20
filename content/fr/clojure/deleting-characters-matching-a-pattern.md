---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Supprimer des caractères correspondant à un motif (pattern) est la tâche de repérer et d'éliminer des characters d'une chaîne qui suivent un certain pattern. Les programmeurs le font pour nettoyer et manipuler des données, souvent pour faciliter la comparaison de chaînes ou pour préparer une entrée utilisateur.

## Comment Faire ?

Clojure offre une fonction puissante appelée `clojure.string/replace` pour réaliser cela. Voici quelques exemples:

```Clojure
(require '[clojure.string :as str])

;; Supprimer tous les caractères numériques dans une chaîne
(str/replace "Salut123, comment4 ça va?" #"[0-9]" "")
;; Résultat : "Salut, comment ça va?"

;; Supprimer tous les caractères non-alphabétiques dans une chaîne
(str/replace "Salut123, comment ça va?" #"[^A-Za-z ]" "")
;; Résultat : "Salut comment a va"
```

## Approfondissement 

Supprimer des pattern a été intégré à la programmation depuis les premiers jours du traitement de texte. C'est une pratique commune maintenant dans de nombreuses langues de programmation, y compris Clojure.

Il y a d'autres manières d'atteindre le même résultat en Clojure. Par exemple, vous pouvez utiliser `filter` avec une fonction personnalisée, ou utiliser `re-seq` pour récupérer tous les caractères qui ne correspondent pas à un pattern et les agréger.

La mise en œuvre de la suppression de caractères correspondant à un motif dans Clojure est basée sur l'API Java String, rendant la fonction à la fois efficace et fiable.

## Voir Aussi 

Pour apprendre davantage sur la bibliothèque `clojure.string`, consultez la documentation officielle [ici](https://clojuredocs.org/clojure.string).

Pour vous renseigner plus sur les expressions régulières utilisées pour correspondre à des motifs, consultez [ce guide](https://www.regular-expressions.info/).