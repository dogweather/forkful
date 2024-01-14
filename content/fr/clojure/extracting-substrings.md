---
title:    "Clojure: Extraction de sous-chaînes"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Pourquoi extraire des sous-chaînes en Clojure

Il existe plusieurs raisons pour lesquelles un programmeur en Clojure pourrait vouloir extraire des sous-chaînes d'une chaîne de caractères. Cela peut être utile pour effectuer des recherches, manipuler et traiter des données, ou encore pour créer des fonctions réutilisables.

## Comment faire

Voici quelques exemples de code en Clojure qui montrent comment extraire des sous-chaînes.

```Clojure
(def my-string "Je suis un programmeur en Clojure")

;; Extraire une sous-chaîne à partir de l'index 7
(subs my-string 7)
;; Output: "un programmeur en Clojure"

;; Extraire une sous-chaîne de 8 caractères à partir de l'index 7
(subs my-string 7 15)
;; Output: "un progr"

;; Extraire une sous-chaîne à partir de la fin de la chaîne
(subs my-string -8)
;; Output: "Clojure"

;; Extraire une sous-chaîne à partir d'une condition
(subs my-string (.indexOf my-string "programmeur") (.indexOf my-string "en"))
;; Output: "programmeur"

```

## Profondeur de plongée

Lors de l'extraction de sous-chaînes en Clojure, il est important de comprendre que les indices commencent à partir de 0, contrairement aux indices de chaînes de caractères en français qui commencent à partir de 1. De plus, il est possible d'utiliser des expressions régulières pour extraire des sous-chaînes à partir de motifs spécifiques.

Voici un exemple d'utilisation d'une expression régulière pour extraire des mots à partir d'une chaîne de caractères:

```Clojure
(def my-string "Bonjour tout le monde")

(re-find #"Bonjour (.*)" my-string)
;; Output: ["Bonjour tout le monde" "tout le monde"]
```

En comprenant ces concepts, il est possible d'extraire des sous-chaînes de manière plus précise et efficace en utilisant les bonnes méthodes et les bonnes expressions régulières.

## Voir aussi

- [Documentation officielle de Clojure pour les fonctions de manipulation de chaînes](https://clojure.org/reference/strings)
- [Site officiel de Clojure](https://clojure.org/)