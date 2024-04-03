---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:30.099619-07:00
description: "Comment faire : Clojure, restant fid\xE8le \xE0 ses racines dans la\
  \ famille Lisp, offre un riche ensemble de fonctions qui s'interface de mani\xE8\
  re transparente\u2026"
lastmod: '2024-03-13T22:44:57.271297-06:00'
model: gpt-4-0125-preview
summary: "Clojure, restant fid\xE8le \xE0 ses racines dans la famille Lisp, offre\
  \ un riche ensemble de fonctions qui s'interface de mani\xE8re transparente avec\
  \ les capacit\xE9s des expressions r\xE9guli\xE8res de Java."
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

## Comment faire :
Clojure, restant fidèle à ses racines dans la famille Lisp, offre un riche ensemble de fonctions qui s'interface de manière transparente avec les capacités des expressions régulières de Java. Voici comment vous pouvez les utiliser :

### Correspondance de Base
Pour vérifier si une chaîne correspond à un motif, utilisez `re-matches`. Il renvoie la correspondance complète si réussi ou `nil` sinon.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Recherche de Motifs
Pour trouver la première occurrence d'un motif, `re-find` est votre fonction de prédilection :

```clojure
(re-find #"\d+" "Commande 123")  ;=> "123"
```

### Groupes de Capture
Utilisez `re-find` avec des parenthèses dans votre motif pour capturer des groupes :

```clojure
(let [[_ zone code] (re-find #"(1)?(\d{3})" "Téléphone : 123-4567")]
  (println "Code Régional:" zone "Code:" code))
;; Sortie : Code Régional: nil Code: 123
```

### Recherche Globale (Trouver Tous les Correspondances)
Clojure n’a pas de recherche globale intégrée comme certains langages. À la place, utilisez `re-seq` pour obtenir une séquence paresseuse de toutes les correspondances :

```clojure
(re-seq #"\d+" "id : 123, qté : 456")  ;=> ("123" "456")
```

### Fractionnement de Chaînes
Pour fractionner une chaîne basée sur un motif, utilisez `clojure.string/split` :

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### Remplacement
Remplacez des parties d'une chaîne correspondant à un motif avec `clojure.string/replace` :

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "AAAA")  ;=> "AAAA-04-01"
```

### Bibliothèques tierces
Bien que le support intégré de Clojure suffise dans la plupart des cas, pour des scénarios plus complexes, envisagez d'utiliser des bibliothèques telles que `clojure.spec` pour une validation de données robuste et `reagent` pour la manipulation réactive du DOM dans des applications web avec routage basé sur des regex et validation d'entrée.

```clojure
;; Exemple d'utilisation de clojure.spec pour valider un email
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valide? ::email "test@example.com")  ;=> vrai
```

Rappelez-vous, bien que les expressions régulières soient puissantes, elles peuvent aussi rendre le code difficile à lire et à maintenir. Utilisez-les judicieusement et envisagez toujours des fonctions de manipulation de chaînes plus simples lorsque c'est possible.
