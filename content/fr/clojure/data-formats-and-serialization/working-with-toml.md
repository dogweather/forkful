---
date: 2024-01-26 04:20:25.463370-07:00
description: "Comment faire : Pour travailler avec TOML dans Clojure, vous avez besoin\
  \ d'une biblioth\xE8que comme `clj-toml`. Ajoutez-la d'abord \xE0 votre `deps.edn`\
  \ ."
lastmod: '2024-03-13T22:44:57.307071-06:00'
model: gpt-4-0125-preview
summary: "Pour travailler avec TOML dans Clojure, vous avez besoin d'une biblioth\xE8\
  que comme `clj-toml`."
title: Travailler avec TOML
weight: 39
---

## Comment faire :
Pour travailler avec TOML dans Clojure, vous avez besoin d'une bibliothèque comme `clj-toml`. Ajoutez-la d'abord à votre `deps.edn` :

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

Ensuite, parsez du TOML :

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'Exemple TOML'")

(def parsed-config (toml/parse-string config-str))

;; Obtenir le titre du TOML parsé
(println (:title parsed-config)) ;; Sortie : Exemple TOML
```

Pour générer du TOML :

```clojure
(def data {:title "Exemple TOML"})

(println (toml/generate-string data))
;; Sortie : title = "Exemple TOML"
```

## Plongée Profonde
TOML a été créé en 2013 par Tom Preston-Werner, co-fondateur de GitHub, comme une alternative plus simple à YAML et JSON pour les fichiers de configuration. Il vise la clarté et a l'intention d'être une spécification que les humains peuvent lire sans outils supplémentaires.

Alors que JSON est souvent utilisé pour les API et les applications web, et que YAML peut devenir complexe avec des références et des capacités de script, TOML se distingue par un focus sur les structures simples et basées sur des tableaux. Cette simplicité le rend particulièrement populaire dans la communauté Rust et d'autres environnements de langages modernes.

Clojure, avec son accent sur la simplicité et la praticité, se marie bien avec TOML pour la configuration. `clj-toml` ou d'autres bibliothèques font le pont. Ils traduisent les données statiques de TOML dans le monde dynamique et fonctionnel de Clojure.

## Voir également
- Repo GitHub de TOML : [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` sur Clojars : [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Docs Clojure : [clojure.org](https://clojure.org/guides/getting_started)
- Intro à `clj-toml` : [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
