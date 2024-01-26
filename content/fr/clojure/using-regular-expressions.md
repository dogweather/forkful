---
title:                "Utilisation des expressions régulières"
html_title:           "Bash: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières permettent de chercher et manipuler du texte selon des motifs. Les programmeurs s’en servent pour valider, chercher, et éditer des données textuelles rapidement.

## Comment faire :
```Clojure
;; Recherche d'une correspondance
(re-find #"\b[Cc]lojure\b" "J'adore programmer en Clojure.")
;; => "Clojure"

;; Vérification de format pour un numéro de téléphone en France
(defn valide-numero? [numero]
  (re-matches #"\+33\s?(\d{1,2}\s?){4}" numero))

(valide-numero? "+33 1 23 45 67 89")
;; => "+33 1 23 45 67 89"

;; Division d'une chaîne en utilisant une virgule comme séparateur
(clojure.string/split "a,b,c" #",")
;; => ["a" "b" "c"]

;; Remplacement de toutes les occurrences de "chat" par "chien"
(clojure.string/replace "Le chat mange. Le chat dort." #"\bchat\b" "chien")
;; => "Le chien mange. Le chien dort."
```

## Exploration en profondeur
Les expressions régulières ont été inventées dans les années 1950 par le mathématicien Stephen Kleene. Clojure, un langage moderne sur la JVM, les utilise via l'interopérabilité Java. Des alternatives existent, comme `clojure.spec`, mais les expressions régulières sont souvent plus rapides pour les besoins simples. L'utilisation sous Clojure passe par deux éléments clés : des littéraux regex (`#"..."`) et des fonctions de la bibliothèque comme `re-find`, `re-matches`, etc.

## Voir Aussi
- La documentation officielle de Clojure sur les patterns regex : [clojure.org](https://clojure.org/guides/learn/functions#_regex)
- "Mastering Clojure Strings", pour une étude plus approfondie des opérations sur les chaînes : [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
- Un tutoriel interactif pour apprendre les regex : [RegexOne](https://regexone.com/)
