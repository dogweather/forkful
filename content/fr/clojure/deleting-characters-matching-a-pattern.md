---
title:                "Supprimer des caractères correspondant à un motif"
html_title:           "Clojure: Supprimer des caractères correspondant à un motif"
simple_title:         "Supprimer des caractères correspondant à un motif"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Supprimer des caractères correspondant à un motif en Clojure : Pourquoi et comment le faire ?

## Qu'est-ce que c'est et pourquoi les programmeurs le font ?
Supprimer des caractères correspondant à un motif en Clojure signifie supprimer toutes les occurrences de caractères qui correspondent à un motif donné dans une chaîne de caractères. Les programmeurs le font pour nettoyer et transformer des données en supprimant des informations superflues ou pour faciliter la manipulation de ces données dans leur programme.

## Comment le faire :
```Clojure
; Supprimer les espaces dans une chaîne de caractères
(def string "Bonjour le monde !")
; Utiliser la fonction str/replace pour remplacer les espaces par une chaîne vide
(def cleaned-string (str/replace string #" " ""))
; Résultat : "Bonjourlemonde!"
```

## Approfondissement :
Supprimer des caractères correspondant à un motif est une technique souvent utilisée en traitement de données et en manipulation de chaînes de caractères. Cette fonctionnalité existe aussi dans d'autres langages de programmation tels que Python ou Java. En utilisant des expressions régulières, il est possible de supprimer non seulement des caractères, mais aussi des mots ou des phrases entières qui correspondent à un motif donné.

## Voir aussi :
- Documentation Clojure : https://clojure.org
- Tutoriel sur les expressions régulières en Clojure : https://clojure.org/guides/learn/syntax#_regular_expressions
- Autres langages de programmation offrant la fonctionnalité de suppression de caractères correspondant à un motif : Python, Java, Perl.