---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
La génération de nombres aléatoires est le processus de production de nombres qui ne suit aucune règle ou modèle précis, de façon totalement imprévisible. Les programmeurs le font pour introduire de l'élément aléatoire dans un programme, comme des événements de jeu imprévisibles ou des clés de cryptage uniques.

## Comment faire :
Voici comment générer un nombre aléatoire en Clojure :
```Clojure
; Générer un nombre aléatoire entre 0 (inclus) et 1 (exclus)
(defn random-number []
  (rand))
```

La sortie de cet exemple serait un nombre aléatoire, comme `0.4975399200724516`.

Pour générer un nombre aléatoire entre deux autres nombres :
```Clojure
; Générer un nombre aléatoire entre min (inclus) et max (exclus)
(defn random-between [min max]
  (+ min (rand (- max min))))
```

En entrant `(random-between 1 10)`, vous pourrez obtenir un nombre aléatoire entre 1 et 10, par exemple `3.6873064530488574`.

## Plongée profonde :
Historiquement, en informatique, il était difficile de générer des nombres véritablement aléatoires. Les ordinateurs sont conçus pour être prévisibles et suivre des instructions précises, pas pour être aléatoires. Progressivement, les algorithmes se sont améliorés, mais ils génèrent toujours ce qu'on appelle des nombres "pseudo-aléatoires", basés sur une initialisation ou "graines" spécifiées.

En Clojure, la fonction `rand` utilise une telle graine pour générer des nombres. La graine par défaut est l'heure système actuelle, mais vous pouvez la modifier en utilisant la fonction `rand-seed`.

Des alternatives à l'utilisation de `rand` comprennent l'utilisation de bibliothèques externes offrant des algorithmes de génération de nombres aléatoires plus robustes, comme java.util.Random.

Les détails d'implémentation de `rand` en Clojure, bien que cachés à l'utilisateur, sont essentiels pour comprendre le comportement et les limites de la génération de nombres aléatoires.

## Voir aussi :
Pour plus d'informations sur la génération de nombres aléatoires en Clojure, consultez les sources suivantes :
- Documentation officielle de Clojure : https://clojuredocs.org/
- Discussion sur StackOverflow : https://stackoverflow.com/questions/27461352/random-seed-in-clojure
- Blog sur les nombres aléatoires en Clojure : https://www.tutorialspoint.com/clojure/clojure_numbers.htm