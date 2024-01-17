---
title:                "Concaténer des chaînes de caractères"
html_title:           "Clojure: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi?

La concaténation de chaînes de caractères est un processus qui consiste à joindre plusieurs chaînes pour créer une seule chaîne de caractères plus longue. Les programmeurs utilisent la concaténation de chaînes pour combiner du texte, des variables et d'autres données en une seule chaîne de caractères.

## Comment faire:

Voici un exemple de concaténation de chaînes en Clojure:
```Clojure
;; Définir deux chaînes
(def str1 "Bonjour")
(def str2 "monde")

;; Utiliser la fonction str pour concaténer les deux chaînes
(str str1 str2)  ; Résultat: "Bonjourmonde"
```

Une autre façon de concaténer des chaînes est d'utiliser l'opérateur `+`:
```Clojure
(def str1 "Bonjour")
(def str2 "monde")

;; Concaténer avec l'opérateur +
(+ str1 str2)  ; Résultat: "Bonjourmonde"
```

Vous pouvez également concaténer plus de deux chaînes en utilisant l'opérateur `str` multiple fois:
```Clojure
(def str1 "Bonjour")
(def str2 "à")
(def str3 "tous")

;; Utiliser plusieurs str pour concaténer les chaînes
(str str1 str2 str3 "!")  ; Résultat: "Bonjour à tous!"
```

## Plongée en profondeur:

Historiquement, la concaténation de chaînes était utilisée principalement pour créer des messages d'erreur et des journaux. Avec l'évolution de la programmation et l'introduction de concepts tels que la programmation orientée objet, d'autres méthodes de manipulation de chaînes de caractères ont émergé. Par exemple, en Clojure, la concaténation peut également être réalisée en utilisant des fonctions telles que `str/join` ou `str/replace`. Cependant, la concaténation avec `str` ou l'opérateur `+` reste la méthode la plus courante et la plus simple pour la plupart des tâches de manipulation de chaînes.

## Voir aussi:

- Documentation officielle Clojure sur la concaténation de chaînes: https://clojuredocs.org/clojure.string/concat
- Un tutoriel sur la concaténation de chaînes en Clojure: https://www.tutorialspoint.com/clojure/clojure_concatenation.htm