---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:10.850610-07:00
description: "Travailler avec le JSON (JavaScript Object Notation) en Clojure implique\
  \ de parser des cha\xEEnes JSON en structures de donn\xE9es Clojure (maps, vecteurs)\
  \ et\u2026"
lastmod: '2024-02-25T18:49:54.190325-07:00'
model: gpt-4-0125-preview
summary: "Travailler avec le JSON (JavaScript Object Notation) en Clojure implique\
  \ de parser des cha\xEEnes JSON en structures de donn\xE9es Clojure (maps, vecteurs)\
  \ et\u2026"
title: Travailler avec JSON
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Travailler avec le JSON (JavaScript Object Notation) en Clojure implique de parser des chaînes JSON en structures de données Clojure (maps, vecteurs) et vice versa. Cette tâche est fondamentale pour les services web, les API et les applications qui doivent communiquer des données dans un format texte structuré, car le JSON est universellement reconnu et pris en charge dans différents environnements de programmation.

## Comment faire :
Clojure n'inclut pas de fonctions intégrées pour travailler avec le JSON, vous utiliserez donc généralement des bibliothèques tierces. `cheshire` et `jsonista` sont des choix populaires en raison de leur facilité d'utilisation et de leur performance.

### Utiliser Cheshire
Tout d'abord, ajoutez Cheshire à vos dépendances de projet dans `project.clj` :
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

Pour parser une chaîne JSON en une map Clojure et convertir une map en chaîne JSON :

```clj
(require '[cheshire.core :as json])

;; Parser la chaîne JSON en map Clojure
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Convertir la map Clojure en chaîne JSON
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Utiliser Jsonista
Ajoutez Jsonista à votre projet `project.clj` :
```clj
[jsonista "0.3.2"]
```

Opérations similaires avec Jsonista :

```clj
(require '[jsonista.core :as j])

;; Parser la chaîne JSON en Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Convertir la map Clojure en chaîne JSON
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

Dans les deux bibliothèques, vous avez la possibilité d'encoder et de décoder des structures de données plus complexes, et il existe des fonctions supplémentaires et des paramètres qui permettent la personnalisation des processus de sérialisation et de désérialisation. Pour la plupart des applications, les fonctionnalités démontrées fournissent une base solide pour travailler avec le JSON dans les applications Clojure.
